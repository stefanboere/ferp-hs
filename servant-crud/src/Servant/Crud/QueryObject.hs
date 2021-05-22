{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module: Servant.Crud.QueryObject
Description: Use any data type as query flags.

This module exposes 'QueryObject', which allows you to use a data type as QueryParam.
Instead of

> type API = QueryParam "author" Text :> QueryParam "isbn" Text :>
>            QueryParam "title" Text :> QueryParam "publisher" Text :> Get '[ 'JSON ] [Book]
>
> booksServer :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Handler [Book]

you get

> type API = QueryObject "" BookQuery :> Get '[ 'JSON ] [ Book ]
>
> booksServer :: BookQuery -> Handler [ Book ]

You don't have to remember if the title of the book was the 3rd or 4th argument.
This makes it especially useful for filters. You can use 'Servant.Crud.QueryOperator' for such behaviour.

This is at the cost of having to declare an extra BookQuery type

> type BookQuery = { author :: Maybe Text, title :: Maybe Text, ... }

If you use the @beam-crud@ library, you get this boilerplate for free, and you will
get a default crud api for free, which does all the filtering, sorting and taking joins
handling for you.

Now go off and read the documentation for 'QueryObject', 'FromQueryText' and 'ToQueryText'.


== Caveats
This is a non-exhaustive list which you can read if you have troubles with this library.
You can probably skip this on first reading.

- 'fromQueryText' is __not__ the inverse of 'toQueryText', it is only
the /left inverse/. This is because there may be multiple
lists of query parameters mapping to the same state.
For instance @foo[]=bar&foo[]=baz@ and @foo=bar&foo=baz@.


- It is also not always the case that 'fromQueryText' is the left inverse of 'toQueryText'.
Consider

> data Role = User (Maybe Int) | Role (Maybe Int)

Then @fromQueryText "" []@ always is @User Nothing@, whereas it might as well have
been @Role Nothing@.
As the user, /you/ should make sure that these conditions do not occur in your data types.


- 'String' is seen as a literal '[Char]':

>>> toQueryText "q" "John"
[("q", "J"), ("q", "o"), ("q", "h"), ("q", "n")]

This corresponds to the query string "?q=J&q=o&q=h&q=n" and not to "?q=John" as
you maybe would expect. Use 'Text' instead.
-}
module Servant.Crud.QueryObject
  (
  -- * Query Objects
    QueryObject
  , QObj
  -- ** FromQueryText
  , FromQueryText
  , fromQueryText
  , defaultFromQueryText
  , ParseResult(..)
  , isAbsent
  , actuallyFound
  -- ** ToQueryText
  , ToQueryText
  , toQueryTextPrio
  , toQueryText
  , defaultToQueryText
  , QueryTextWithPrio
  , Prio
  -- * Parse Options
  , Options(..)
  , defaultOptions
  , fromAesonOptions
  -- * Internals
  , State(..)
  , emptyState
  , mapS
  , addCPrefix
  , addSPrefix
  , toParamName
  -- * Generics
  , GFromQueryText
  , GToQueryText
  )
where

import           Prelude

import qualified Data.Aeson                    as Aeson
import qualified Data.Bifunctor                 ( first )
import           Data.Either                    ( partitionEithers )
import qualified Data.List                     as L

import           Data.Maybe                     ( mapMaybe )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Typeable                  ( Typeable )
import           GHC.Generics
import           GHC.TypeLits                   ( KnownSymbol
                                                , Symbol
                                                , symbolVal
                                                )
import           Network.HTTP.Types             ( QueryText )
import           Servant.API
import           Servant.Links
import           Servant.Client.Core            ( HasClient(..)
                                                , appendToQueryString
                                                )

-- | This type replaces 'QueryParam' or 'QueryFlag' or 'QueryParams'
--
-- It is like these parameters, except it accepts more general types.
--
-- > type BookQuery = BookQuery { author :: Maybe Text, isbn :: Maybe Text }
-- >          deriving (Generic, FromQueryText, ToQueryText)
-- >
-- > type BooksApi = QueryObject "" BookQuery :> Get '[ 'JSON ] [ Book ]
-- >
-- > bookServer :: BookQuery -> Handler [ Book ]
-- > ...
--
-- Then requesting @/books?author=Foo@ results in
--
-- > BooksQuery { author = Just "Foo", isbn = Nothing }

-- passed to @bookServer@.
--
-- The 'Symbol' @sym@ is a constant which is prefixed to the parameter name.
--
-- > type BooksApi' = QueryObject "prefix" BookQuery :> Get '[ 'JSON ] [ Book ]
--
-- Then you would need to request @/books?prefix.author=Foo@.
data QueryObject (sym :: Symbol) (a :: *)
  deriving Typeable

-- | Shortcut for 'QueryObject'  with no prefix
type QObj = QueryObject ""

-- | Like 'Either' but also keeps track of missing data
data ParseResult a =
    Found a
  | ParseError Text
  | Absent a -- ^ It is absent, but here is a default value
  deriving (Show, Eq)

-- | Whether the value is absent
isAbsent :: ParseResult a -> Bool
isAbsent (Absent _) = True
isAbsent _          = False

-- | Mark absent as found
actuallyFound :: ParseResult a -> ParseResult a
actuallyFound (Absent x) = Found x
actuallyFound x          = x

instance Functor ParseResult where
  fmap f (Found      y) = Found (f y)
  fmap f (Absent     y) = Absent (f y)
  fmap _ (ParseError y) = ParseError y

instance Applicative ParseResult where
  pure = Found
  ParseError e <*> _ = ParseError e
  Absent     f <*> r = fmap f r
  Found      f <*> r = actuallyFound $ fmap f r

instance Monad ParseResult where
  ParseError l >>= _ = ParseError l
  Absent     r >>= k = k r
  Found      r >>= k = actuallyFound $ k r

fromParseResult :: Either Text a -> ParseResult a
fromParseResult (Right y) = Found y
fromParseResult (Left  y) = ParseError y

-- | Types which can be parsed from query text
--
-- You probably want to use Generics to derive instances.
--
-- Basically it tries to lookup a parameter by it's name, which is by default
-- the record field names to access the field, separated by a dot.
--
-- > data Foo = Foo { unFoo :: Bar }
-- > data Bar = Bar { unBar :: Bool }
--
-- Then @?unFoo.unBar@ gets parsed as @ Foo { unFoo = Bar { unBar = True }}@.
-- Use 'Options' with 'defaultFromQueryText' to alter this behaviour, for example,
-- to add a field label modifier which drops the "un" prefix.
--
-- For sum types the constructor name is not dropped. This allows for the following behaviour.
--
-- > data Color = Red | Blue | Green
--
-- Then @?blue@ gets parsed as @Blue@, etc.
-- If none are present it will just pick the first, in this case @Red@. If that is
-- undesired you should wrap it in a 'Maybe' or List.
class FromQueryText a where
  -- | Parses a list of Query Parameters given a certain prefix
  -- If the first argument is nonempty, it is also added as a prefix.
  fromQueryText :: Text -> QueryText -> ParseResult a
  default fromQueryText :: (Generic a, GFromQueryText (Rep a)) => Text -> QueryText -> ParseResult a
  fromQueryText = defaultFromQueryText defaultOptions

-- | Generic implementation of 'fromQueryText'.
--
-- Runs 'gFromQueryText' with the empty state
defaultFromQueryText
  :: (Generic a, GFromQueryText (Rep a))
  => Options
  -> Text
  -> QueryText
  -> ParseResult a
defaultFromQueryText opts pref q = to
  <$> gFromQueryText (emptyState pref) opts (normalize q)
  where normalize = map (Data.Bifunctor.first Text.toCaseFold)

instance FromQueryText () where
  fromQueryText paramname querytext = case lookup paramname querytext of
    Just _  -> Found () -- param is there
    Nothing -> Absent ()

instance FromQueryText (U1 p) where
  fromQueryText paramname querytext = case lookup paramname querytext of
    Just _  -> Found U1 -- param is there
    Nothing -> Absent U1

-- | Parses a boolean. The presence results in True, the absence in False
instance FromQueryText Bool where
  fromQueryText paramname querytext = case lookup paramname querytext of
    Just Nothing  -> Found True -- param is there, with no value
    Just (Just v) -> examine v -- param with value
    Nothing       -> Absent False
   where
    examine v | v == "true" || v == "1" || v == "" = Found True
              | otherwise                          = Absent False

-- | Parses a list of query parameters.
instance FromHttpApiData a => FromQueryText [a] where
  fromQueryText paramname querytext =
    case partitionEithers $ fmap parseQueryParam ps of
      ([], parsed) -> if null parsed then Absent [] else Found parsed
      (errs, _) ->
        ParseError $ paramname <> " failed: " <> Text.intercalate ", " errs
   where
    ps :: [Text]
    ps = mapMaybe snd . filter (looksLikeParam . fst) $ querytext

    looksLikeParam name = name == paramname || name == (paramname <> "[]")

-- | If the parameter is given parse it, else return Nothing
instance FromHttpApiData a => FromQueryText (Maybe a) where
  fromQueryText paramname querytext = parseParam $ lookup paramname querytext
   where
    parseParam :: Maybe (Maybe Text) -> ParseResult (Maybe a)
    parseParam (Just (Just x)) = fromParseResult $ Just <$> parseQueryParam x
    parseParam (Just Nothing ) = fromParseResult $ Just <$> parseQueryParam ""
    parseParam Nothing         = Absent Nothing

-- | Options for 'gFromQueryText' and 'defaultFromQueryText'
--
-- Use 'fieldLabelModifier' to drop a common record prefix.
data Options = Options
  { fieldLabelModifier :: String -> String -- ^ Function applied to field labels, e.g. for removing labels
  , constructorTagModifier :: String -> String -- ^ Function applied to constructor tags, e.g. for removing the tag
  , unwrapUnaryRecords :: Bool -- ^ Hide the field name when a record constructor has only one field, like a newtype
  , unwrapUnaryConstructors :: Bool -- ^ Hide the constructor name for a datatype with a single constructor
  }

-- | Default encoding and decoding 'Options'
--
-- > Options
-- >  { fieldLabelModifier = id
-- >  , constructorTagModifier = id
-- >  , unwrapUnaryRecords = False
-- >  , unwrapUnaryConstructors = True
-- >  }
defaultOptions :: Options
defaultOptions = Options { fieldLabelModifier      = id
                         , constructorTagModifier  = id
                         , unwrapUnaryRecords      = False -- For compatibility with aeson
                         , unwrapUnaryConstructors = True
                         }

-- | Convert 'Aeson.Options' to 'Options' with defaults given by 'defaultOptions'
fromAesonOptions :: Aeson.Options -> Options
fromAesonOptions x = defaultOptions
  { fieldLabelModifier     = Aeson.fieldLabelModifier x
  , constructorTagModifier = Aeson.constructorTagModifier x
  , unwrapUnaryRecords     = Aeson.unwrapUnaryRecords x
  }

-- | Parse state
data State = State
  { prefixes :: [ String ] -- ^ Prefixes to append, in reverse order, will be intercalated with dots
  , dropNext :: Int -- ^ How many times not to add the prefix
  }

-- | The 'Show' instance of 'State'
toParamName :: State -> Text
toParamName s =
  Text.toCaseFold
    . Text.intercalate "."
    . reverse
    . map Text.pack
    . filter (not . null)
    $ prefixes s

-- | Inital parsing state
emptyState :: Text -> State
emptyState pref = State [ Text.unpack pref | not (Text.null pref) ] 0

-- | Add a prefix to the 'State'
addPrefix :: String -> State -> State
addPrefix str state
  | dropNext state <= 0 = state { prefixes = str : prefixes state }
  | otherwise           = state { dropNext = dropNext state - 1 }

-- | Prevent next prefix to be added
preventNext :: State -> State
preventNext state = state { dropNext = dropNext state + 1 }

-- | Map state using the options
-- Prevents the next prefix to be added or does nothing depending on options
mapS :: Options -> State -> State
mapS opts = if unwrapUnaryConstructors opts then preventNext else id

-- | Add the constructor name
addCPrefix :: Constructor c => C1 c f () -> Options -> State -> State
addCPrefix c1 opts = addPrefix (constructorTagModifier opts (conName c1))

-- | Add the selector name
addSPrefix :: Selector s => S1 s f () -> Options -> State -> State
addSPrefix s1 opts = addPrefix (fieldLabelModifier opts (selName s1))

-- | FromQueryText for types of kind (* -> *)
class GFromQueryText (a :: * -> *) where
  gFromQueryText :: State
    -> Options
    -> QueryText  -- ^ Query text to parse
    -> ParseResult (a p) -- ^ Parse result or error

-- | Parse unary data types
instance {-# OVERLAPPING #-} (GFromQueryText f, Constructor c)
  => GFromQueryText (D1 t (C1 c f)) where
  gFromQueryText s opts q = M1 <$> gFromQueryText (mapS opts s) opts q

-- | Discard data type meta information for all other data types
instance GFromQueryText f
  => GFromQueryText (D1 t f) where
  gFromQueryText s opts q = M1 <$> gFromQueryText s opts q

-- | Add the constructor name, optionally ignore unaryConstructors
instance {-# OVERLAPPING #-} (GFromQueryText f, Constructor c, Selector s)
  => GFromQueryText (C1 c (S1 s f)) where
  gFromQueryText s opts q = M1 <$> gFromQueryText (mapS opts s') opts q
    where s' = addCPrefix (undefined :: C1 c f ()) opts s

-- | Add the constructor name to the list of path segments for all other constructors
instance (GFromQueryText f, Constructor c)
  => GFromQueryText (C1 c f) where
  gFromQueryText s opts q = M1 <$> gFromQueryText s' opts q
    where s' = addCPrefix (undefined :: C1 c f ()) opts s

-- | Add selector to the list of prefixes
instance (GFromQueryText f, Selector s)
  => GFromQueryText (S1 s f) where
  gFromQueryText s opts q = M1 <$> gFromQueryText s' opts q
    where s' = addSPrefix (undefined :: S1 s f ()) opts s

-- | For products we just need both
instance (GFromQueryText f, GFromQueryText g)
  => GFromQueryText (f :*: g) where
  gFromQueryText s opts q =
    gFromQueryText s opts q >>= (\x -> (x :*:) <$> gFromQueryText s opts q)

-- | For sums we return the first one which succeeds
instance (GFromQueryText f, GFromQueryText g)
  => GFromQueryText (f :+: g) where
  gFromQueryText s opts q = case gFromQueryText s opts q of
    ParseError t -> ParseError t
    Absent     _ -> L1 <$> gFromQueryText s opts q
    Found      y -> R1 <$> Found y

-- | This is a wrapper for the inner most datatype. So we just apply the fromQueryText instance
instance (FromQueryText c) => GFromQueryText (K1 i c) where
  gFromQueryText s _opts q = K1 <$> fromQueryText (toParamName s) q

-- | The inner most wrapper might also be a Unit
instance GFromQueryText U1 where
  gFromQueryText s _opts = fromQueryText (toParamName s)

-- | Attach a priority. Lower comes first.
type Prio = Int

-- | 'QueryText' with ordering information attached
type QueryTextWithPrio = [(Prio, (Text, Maybe Text))]

-- | Types which can be printed as query text
--
-- See 'FromQueryText' how the param name works.
class ToQueryText a where
  -- | Parses a list of Query Parameters given a certain prefix
  --
  -- It returns a list of query parameters, but, since ordering might be relevant,
  -- you can assign a priority to each element. This is especially important if you are doing
  -- something like a sort parameter, where column names can be specified to order by.
  toQueryTextPrio :: Text -> a -> QueryTextWithPrio
  default toQueryTextPrio :: (Generic a, GToQueryText (Rep a)) => Text -> a -> QueryTextWithPrio
  toQueryTextPrio = defaultToQueryText defaultOptions

-- | Runs 'toQueryTextPrio' and then sorts the result properly
toQueryText :: ToQueryText a => Text -> a -> QueryText
toQueryText t = map snd . L.sortOn fst . toQueryTextPrio t

-- | Runs 'gToQueryText' with the empty state
defaultToQueryText
  :: (Generic a, GToQueryText (Rep a))
  => Options
  -> Text
  -> a
  -> QueryTextWithPrio
defaultToQueryText opts prefix x =
  gToQueryText (emptyState prefix) opts (from x)

instance ToQueryText (U1 p) where
  toQueryTextPrio paramname U1 = [(0, (paramname, Nothing))]

instance ToQueryText () where
  toQueryTextPrio paramname () = [(0, (paramname, Nothing))]

instance ToQueryText Bool where
  toQueryTextPrio paramname True  = [(0, (paramname, Nothing))]
  toQueryTextPrio _         False = []

instance ToHttpApiData a => ToQueryText (Maybe a) where
  toQueryTextPrio _ Nothing = []
  toQueryTextPrio paramname (Just x) =
    [(0, (paramname, Just $ toQueryParam x))]

instance ToHttpApiData a => ToQueryText [a] where
  toQueryTextPrio paramname =
    map (\x -> (0, (paramname, Just $ toQueryParam x)))

-- | 'ToQueryText' for types of kind (* -> *)
class GToQueryText (a :: * -> *) where
  gToQueryText :: State
    -> Options
    -> a p -- ^ Object to print
    -> QueryTextWithPrio -- ^ List of query parameters


-- | Print unary data types
instance {-# OVERLAPPING #-} (GToQueryText f, Constructor c)
  => GToQueryText (D1 t (C1 c f)) where
  gToQueryText s opts (M1 x) = gToQueryText (mapS opts s) opts x

-- | Discard data type meta information for all other data types
instance GToQueryText f
  => GToQueryText (D1 t f) where
  gToQueryText s opts (M1 x) = gToQueryText s opts x

-- | Add the constructor name, optionally ignore unaryConstructors
instance {-# OVERLAPPING #-} (GToQueryText f, Constructor c, Selector s)
  => GToQueryText (C1 c (S1 s f)) where
  gToQueryText s opts (M1 x) = gToQueryText (mapS opts s') opts x
    where s' = addCPrefix (undefined :: C1 c f ()) opts s

-- | Add the constructor name to the list of path segments for all other constructors
instance (GToQueryText f, Constructor c)
  => GToQueryText (C1 c f) where
  gToQueryText s opts (M1 x) = gToQueryText s' opts x
    where s' = addCPrefix (undefined :: C1 c f ()) opts s

-- | Add selector to the list of prefixes
instance (GToQueryText f, Selector s)
  => GToQueryText (S1 s f) where
  gToQueryText s opts (M1 x) = gToQueryText s' opts x
    where s' = addSPrefix (undefined :: S1 s f ()) opts s

-- | For products we just print both
instance (GToQueryText f, GToQueryText g)
  => GToQueryText (f :*: g) where
  gToQueryText s opts (x :*: y) =
    gToQueryText s opts x ++ gToQueryText s opts y

-- | For sums we print either one of them
instance (GToQueryText f, GToQueryText g)
  => GToQueryText (f :+: g) where
  gToQueryText s opts (R1 x) = gToQueryText s opts x
  gToQueryText s opts (L1 x) = gToQueryText s opts x

-- | This is a wrapper for the inner most datatype.
instance (ToQueryText c) => GToQueryText (K1 i c) where
  gToQueryText s _opts (K1 x) = toQueryTextPrio (toParamName s) x

-- | Inner most constant datatype
instance GToQueryText U1 where
  gToQueryText s _opts U1 = toQueryTextPrio (toParamName s) U1


-- INSTANCES

instance (HasClient m api, ToQueryText a, KnownSymbol sym)
    => HasClient m (QueryObject sym a :> api) where
  type Client m (QueryObject sym a :> api) = a -> Client m api

  clientWithRoute pm Proxy req param = clientWithRoute
    pm
    (Proxy :: Proxy api)
    (L.foldl' (\req' (p, v) -> appendToQueryString p v req')
              req
              (toQueryText paramname param)
    )
    where paramname = Text.pack $ symbolVal (Proxy :: Proxy sym)

  hoistClientMonad pm _ f cl as =
    hoistClientMonad pm (Proxy :: Proxy api) f (cl as)


instance (HasLink api, ToQueryText v, KnownSymbol sym)
    => HasLink (QueryObject sym v :> api) where
  type MkLink (QueryObject sym v :> api) a = v -> MkLink api a
  toLink toA _ l =
    toLink toA (Proxy :: Proxy api)
      . L.foldl' (\l' v -> addQueryParam (toParam v) l') l
      . toQueryText paramname

   where
    paramname = Text.pack $ symbolVal (Proxy :: Proxy sym)

    toParam (x, Nothing) = FlagParam (Text.unpack x)
    toParam (x, Just y ) = SingleParam (Text.unpack x) y

    -- FIXME Fix this when addQueryParam is exported from servant
    addQueryParam :: Param -> Link -> Link
    addQueryParam _ = id
