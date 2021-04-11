{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Servant.Crud.QueryOperator
Description: Query filters

This module is to enrich the fields of "Servant.Crud.QueryObject" to support
more complex behaviour, most notably filtering.

In "Servant.Crud.QueryObject" we show how to specify an Api where users can filter data,
based on equalities. @/books?author=Stephen%20King@ only returns books from Stephen King,
i.e. the author name is exactly "Stephen King".
But what if I want to find books newer than 2010? Or books containing `Haskell'?
This module presents a solution.

We will add an \"Operator\" after the field name, but before the =, where additional
information can be stored about the value. The format is @?field[operator]=value@.
Examples:

- @?year[gt]=2010@
- @?title[contains]=Haskell@
- @?author=Stephen%20King@
- @?autor[!]=Stephen%20King@

For an empty operator string you don't have to write the square brackets.

== How to use this?
Insert 'Filter' where you would otherwise put 'Maybe' or '[]':

> type BookFilter = BookFilter { author :: Filter Text, year :: Filter Int, ... }
>       deriving (Show, FromQueryText, ToQueryText)
>
> type BooksApi = QueryObject "" BookFilter :> Get '[ 'JSON ] [Book]

== How to specify which operators to use?
Same types share the same set of valid operators, i.e. we must give a function
from type to a list of operators. This allows us, for example, to only accept ordering
operators such as @gt@ for types which actually can be ordered.

Also, maybe you don't like the word \"contains\", then you can specify another name.
See 'DefaultFilters'.

== How to access the data
You can access a single operator, if you know it is there, with 'getf'.

Suppose you would want to do more, such as converting it in a SQL string, you would
typically use some kind of recursion. Here is an example of implementing an arbitray instance
for the filter.

@
instance Arbitrary (FilterT a) => Arbitrary (Filter a) where
  arbitrary = Filter \<$\> arbitrary
  shrink    = map Filter . shrink . unFilter

instance Arbitrary (OpDict '[] a) where
  arbitrary = pure Nil -- There is nothing to choose from

instance (Arbitrary (OpEntry s k a), Arbitrary (OpDict xs a))
  => Arbitrary (OpDict ('(s, k) ': xs) a) where
  arbitrary = (:>) \<$\> arbitrary \<*\> arbitrary -- Use recursion here

-- Most of the time this is the most complicated instance
instance (KnownSymbol s, KnownParamKind k, Arbitrary (ParamKindFunctor k a))
  => Arbitrary (OpEntry s k a) where
  arbitrary = E \<$\> arbitrary -- This uses the arbitrary instances for either (Maybe a, [a] or TaggedBool a)
@

/Thanks to Stephanie Weirich for supplying an example of something like 'OpDict', <https:\/\/github.com\/sweirich\/dth\/blob\/master\/regexp\/src\/OccDict.hs> /

-}
module Servant.Crud.QueryOperator
  ( -- * Standard filters
    Filter(..)
  , DefaultFilters
  , EqFilter
  , OrdFilter
  , StrFilter
  , AddEqFilter
  , AddOrdFilter
  , AddStrFilter
  , AddNullFilter
  -- * Parameter kinds
  , ParamKindFunctor
  , TaggedBool(..)
  , MaybeLast(..)
  , ParamKind(..)
  -- * Getting and setting data
  , setf
  , getf
  , paramKindVal
  -- * Internals
  , OpDict(..)
  , OpEntry(..)
  , FilterT
  , KnownParamKind
  )
where

import           Prelude

import           Control.Applicative            ( empty
                                                , Alternative(..)
                                                )
import           Data.Default                   ( Default
                                                , def
                                                )
import           Data.Kind                      ( Constraint
                                                , Type
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Semigroup
import           GHC.Records
import           GHC.TypeLits                   ( Symbol
                                                , TypeError
                                                , ErrorMessage(..)
                                                , KnownSymbol
                                                , symbolVal
                                                )
import           Network.HTTP.Types             ( QueryText )
import           Servant.API

import           Servant.Crud.QueryObject


-- Inspired by https://github.com/sweirich/dth/blob/master/regexp/src/OccDict.hs

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (x : _) = Just x


-- | Contains a key value map which maps the operator name to a value.
-- Use this type everywhere where you want filters.
newtype Filter a = Filter { unFilter :: FilterT a }

-- | The real filter type, a list of some Query Operators, where this
-- list is based on the inner type via 'DefaultFilters'
type FilterT a = OpDict (DefaultFilters a) a

instance (FromQueryText (FilterT a)) => FromQueryText (Filter a) where
  fromQueryText paramName qs = Filter <$> fromQueryText paramName qs

instance (ToQueryText (FilterT a)) => ToQueryText (Filter a) where
  toQueryTextPrio paramName (Filter x) = toQueryTextPrio paramName x

instance (Show (FilterT a)) => Show (Filter a) where
  show (Filter x) = show x

instance (Eq (FilterT a)) => Eq (Filter a) where
  (Filter x) == (Filter y) = x == y

instance (Semigroup (FilterT a)) => Semigroup (Filter a) where
  (Filter x) <> (Filter y) = Filter (x <> y)

instance (Monoid (FilterT a)) => Monoid (Filter a) where
  mempty = Filter mempty

instance (Default (FilterT a)) => Default (Filter a) where
  def = Filter def

-- | Using this type level function (in other words a /Type Family/) we specify
-- which operators to use for which types. For maximal flexibility,
-- there are no instances yet, so you will have to create these yourself.
-- Make sure you use the @TypeFamilies@ and @DataKinds@ language extensions.
--
-- Here is the deal: For each type @a@  which appears somewhere as @Filter a@ you write
--
-- > type instance DefaultFilters a = <listOfOperators>
--
-- Here @listOfOperators@ is a list of tuples @(operatorName, paramKind)@, where
-- @operatorName@ is the string which goes in between the square brackets, and
-- @paramKind@ is either 'Flag', 'Normal' or 'List' (see 'ParamKind'). E.g.:
--
-- > type instance DefaultFilters String = '[ '("contains", 'Normal), '("oneOf", 'List), '("null", 'Flag) ]
--
-- To help you along, there are some default lists. You could write
--
-- > type instance DefaultFilters Text = StrFilter
-- > type instance DefaultFilters Int = OrdFilter
-- > type instance DefaultFilters Bool = EqFilter
-- > type instance DefaultFilters (Maybe a) = AddNullFilter (DefaultFilters a)
type family DefaultFilters a :: [(Symbol, ParamKind)]

-- | Add filters for types which have an 'Eq' filters.
--
-- Adds operators \"\" and \"!\", all of type 'List'.
type AddEqFilter (xs :: OpMap) = '("", 'List ) ': '("!", 'List) ': xs

-- | Add filters to filter based on ordering.
--
-- Adds operators \"gt\", \"lt\", \"ge\", \"le\", all of type 'Normal'.
-- brittany-disable-next-binding
type AddOrdFilter (xs :: OpMap)
  = '("gt", 'Normal )
      ':
      '("lt", 'Normal)
      ':
      '("ge", 'Normal)
      ':
      '("le", 'Normal)
      ':
      xs

-- | Add filters for string filtering
--
-- Adds operators \"start\", \"end\", \"contains\", \"match\", \"!start\", \"!end"\
-- \"!contains\" and \"!match\", all of type 'Normal'
-- brittany-disable-next-binding
type AddStrFilter (xs :: OpMap)
  = '("start", 'Normal)
      ':
      '("end", 'Normal)
      ':
      '("contains", 'Normal)
      ':
      '("match", 'Normal)
      ':
      '("!start", 'Normal)
      ':
      '("!end", 'Normal)
      ':
      '("!contains", 'Normal)
      ':
      '("!match", 'Normal)
      ':
      xs

-- | Add filtering for nullable fields
--
-- Adds operators \"null\" and \"!null\", both of type 'Flag'
type AddNullFilter (xs :: OpMap) = '("null", 'Flag ) ': '("!null", 'Flag) ': xs

-- | Just the Eq filters
type EqFilter = AddEqFilter '[]

-- | Eq and Ord filters
type OrdFilter = AddOrdFilter (AddEqFilter '[])

-- | Eq, Ord and String filters
type StrFilter = AddStrFilter (AddOrdFilter (AddEqFilter '[]))


-- | Represents a single entry in the dictionary of operators.
-- It stores the foo in @?title[contains]=foo@.
-- brittany-disable-next-binding
data OpEntry :: Symbol -> ParamKind -> Type -> Type where
  E ::forall s k a . ParamKindFunctor k a -> OpEntry s k a

-- | A list of query operators (names and result types)
type OpMap = [(Symbol, ParamKind)]

-- | Here is how to have multiple query operators.
-- Here are all the query parameters stored, each with the right type (as specified in 'ParamKindFunctor').
data OpDict :: OpMap -> Type -> Type where
  Nil ::OpDict '[] a
  (:>) ::OpEntry s k a -> OpDict ts a -> OpDict ( '(s, k) ': ts ) a

infixr 5 :>

-- | A proof that a symbol appears in the dictionary
data IsInDict (s :: Symbol) ( k :: ParamKind ) ( xs :: OpMap ) where
  DH ::IsInDict s k ( '(s, k) : xs ) -- ^ Either it occurs at the head
  DT ::IsInDict s k xs -> IsInDict x k (t : xs)  -- ^ Or somewhere in the body

-- Find a name n in s, if it exists (and return a proof!),
-- or produce a TypeError
-- NOTE: We need TypeInType to return a GADT from a type family
type Find s xs = (FindH s xs xs :: IsInDict s k xs)

-- | Given a symbol and a list give the proof that the symbol is in the dictionary
-- i.e. give a function to Index
-- The second list is to give meaningfull error messages
type family FindH (s :: Symbol) (xs :: OpMap) (xs2 :: OpMap) :: IsInDict s k xs where
  FindH s ('(s, k) : xs) xs2 = 'DH -- If it is at the head, return the proof that it is in the head
  FindH s ('(t, p) : xs) xs2 = 'DT (FindH s xs xs2) --  Look further
  FindH s '[]            xs2 =
    TypeError ( 'Text "I couldn't find the operator '" ':<>:
                'Text s ':<>: 'Text "' in " ':$$:
                'Text "    {" ':<>: ShowOpMap xs2 ':<>: 'Text "}"
              )

-- | Used to show 'OpMap' as a compile error
type family ShowOpMap (m :: OpMap) :: ErrorMessage where
  ShowOpMap '[] = 'Text ""
  ShowOpMap '[ '(s, k) ] = 'Text s
  ShowOpMap ('(s, k) : m ) = 'Text s ':<>: 'Text ", " ':<>: ShowOpMap m

-- | Lookup an entry in a dict.
-- Only lookup elements which are proven to be in the dict.
-- The element and dictionary together determine the kind.
-- Mostly for internal use.
class GetOp (p :: IsInDict s k xs ) where
  getp :: OpDict xs a -> ParamKindFunctor k a

-- Get the head
instance GetOp 'DH where
  getp (E v :> _) = v
  {-# INLINE getp #-}

-- Get something in the tail
instance (GetOp l) => GetOp ('DT l) where
  getp (_ :> xs) = getp @_ @_ @_ @l xs
  {-# INLINE getp #-}


instance (GetOp (Find s xs :: IsInDict s k xs),
         t ~ ParamKindFunctor k a) => HasField '(s, k) (OpDict xs a) t where
  getField = getp @_ @_ @_ @(Find s xs)
  {-# INLINE getField #-}

-- | Get a filter from the dictionary
-- Usage 'getVal @"" dict'
getVal
  :: forall s k xs a
   . GetOp (Find s xs :: IsInDict s k xs)
  => OpDict xs a
  -> ParamKindFunctor k a
getVal = getp @_ @_ @_ @(Find s xs)

-- | Gets the value from the dictionary. It returns either @Just a@, @[a]@ or @TaggedBool a@,
-- depending on @k@.
--
-- > nullPresent = get @"null" filter
--
-- /The junk before => is just to check that \"null\" is actually in the dictionary./
getf
  :: forall s k a
   . GetOp (Find s (DefaultFilters a) :: IsInDict s k (DefaultFilters a))
  => Filter a
  -> ParamKindFunctor k a
getf = getVal @s . unFilter

-- | Lookup an entry in a dict
-- Only lookup elements which are proven to be in the dict
-- The element and dictionary together determine the kind
class SetOp (p :: IsInDict s k xs ) where
  setp :: ParamKindFunctor k a -> OpDict xs a -> OpDict xs a

-- Set the head
instance SetOp 'DH where
  setp y (E _ :> xs) = E y :> xs
  {-# INLINE setp #-}

-- Set something in the tail
instance (SetOp l) => SetOp ('DT l) where
  setp y (x :> xs) = x :> setp @_ @_ @_ @l y xs
  {-# INLINE setp #-}

-- | Set a value in the filter
-- Usage 'setVal @"null" Present def'
setVal
  :: forall s k xs a
   . SetOp (Find s xs :: IsInDict s k xs)
  => ParamKindFunctor k a
  -> OpDict xs a
  -> OpDict xs a
setVal = setp @_ @_ @_ @(Find s xs)

-- | Sets the value of a filter in filter, basically this is just dictionary update.
-- You need a new value of the right type.
-- For 'Normal' this is (Maybe a), for example. The @TypeApplications@ language
-- extension is needed. Type applications are used to specify which entry we want to modify.
--
-- > newFilter = set @"null" Present oldfilter
--
-- /The junk before => is just to check that \"null\" is actually in the dictionary./
setf
  :: forall s k a
   . SetOp (Find s (DefaultFilters a) :: IsInDict s k (DefaultFilters a))
  => ParamKindFunctor k a
  -> Filter a
  -> Filter a
setf x = Filter . setVal @s x . unFilter

-- | Like a bool, but with an inner type, but no values
data TaggedBool a = Present | NotPresent
  deriving (Show, Eq, Enum, Bounded)

-- | Like the boolean monoid 'Any', with mempty = NotPresent
-- Here '<>' = '||'
instance Semigroup (TaggedBool a) where
  NotPresent <> NotPresent = NotPresent
  _          <> _          = Present

-- | Like the boolean monoid 'Any', with mempty = NotPresent
instance Monoid (TaggedBool a) where
  mempty = NotPresent

-- | the function does nothing, satisfies both functor laws trivially
instance Functor TaggedBool where
  fmap _ Present    = Present
  fmap _ NotPresent = NotPresent

-- | The function is only used for type coercion
instance Applicative TaggedBool where
  pure _ = NotPresent
  _ <*> Present    = Present
  _ <*> NotPresent = NotPresent

-- | '<|>' behaves as '||'
instance Alternative TaggedBool where
  empty = NotPresent
  NotPresent <|> x = x
  Present    <|> _ = Present

instance Default (TaggedBool a) where
  def = NotPresent

-- | A newtype wrapper around 'Maybe a' providing the same functionality as 'Maybe (Last a)'
newtype MaybeLast a = MaybeLast { unMaybeLast :: Maybe a }
  deriving (Show, Eq)
  deriving newtype (Functor, Applicative, Alternative, Default)

instance Semigroup (MaybeLast a) where
  MaybeLast x <> MaybeLast y =
    MaybeLast . fmap getLast $ fmap Last x <> fmap Last y

instance Monoid (MaybeLast a) where
  mempty = MaybeLast Nothing

-- | What kind of query parameter it is (how many times it can appear)
data ParamKind
  = Flag -- ^ Appears at most once without a value
  | Normal -- ^ Appears at most once with a value
  | List -- ^ Appears zero or many times with a value
  deriving (Eq, Ord, Bounded, Show, Enum)

-- | Type family to specify what the input functor is for 'parseQuery', i.e how many times a parameter can
-- appear and if a value should be supplied.
-- This type family is injective
type family ParamKindFunctor (k :: ParamKind) = (res :: Type -> Type) | res -> k where
  ParamKindFunctor 'Normal = MaybeLast
  ParamKindFunctor 'Flag = TaggedBool
  ParamKindFunctor 'List = []

-- | What kind of constraint is needed for 'toQueryText'
type family ToHttpConstraint (k :: ParamKind) b :: Constraint where
  ToHttpConstraint 'Normal b = ToHttpApiData b
  ToHttpConstraint 'List b = ToHttpApiData b
  ToHttpConstraint 'Flag b = (() :: Constraint)

-- | What kind of constraint is needed for 'fromQueryText'
type family FromHttpConstraint (k :: ParamKind) b :: Constraint where
  FromHttpConstraint 'Normal b = FromHttpApiData b
  FromHttpConstraint 'List b = FromHttpApiData b
  FromHttpConstraint 'Flag b = (() :: Constraint)

-- | Singleton for 'ParamKind'
newtype SParamKind (a :: ParamKind) = SParamKind ParamKind

-- | Unlift Param Kind from type level back to data level
class KnownParamKind t where
  paramKindSing :: SParamKind t

-- | Unlift the ParamKind type to the value, i.e. turn the type @t@ into an actual value.
paramKindVal :: forall t proxy . KnownParamKind t => proxy t -> ParamKind
paramKindVal _ = case paramKindSing :: SParamKind t of
  SParamKind x -> x

instance KnownParamKind 'Normal where
  paramKindSing = SParamKind Normal

instance KnownParamKind 'List where
  paramKindSing = SParamKind List

instance KnownParamKind 'Flag where
  paramKindSing = SParamKind Flag

-- 'Show' instances
instance Show (OpDict '[] a) where
  show Nil = "Nil"

instance (Show (OpEntry s k a), Show (OpDict xs a))
    => Show (OpDict ('(s, k) ': xs) a) where
  show (x :> xs) = show x <> " :> " <> show xs

instance (KnownSymbol s, Show (ParamKindFunctor k a))
    => Show (OpEntry s k a) where
  show (E xs) = "{ " <> symbolVal (Proxy :: Proxy s) <> "=" <> show xs <> " }"


-- 'Eq' instance
instance Eq (OpDict '[] a) where
  Nil == Nil = True

instance (Eq (OpEntry s k a), Eq (OpDict xs a))
  => Eq (OpDict ('(s,k) ': xs) a ) where
  (x :> xs) == (y :> ys) = x == y && xs == ys

instance (KnownSymbol s, KnownParamKind k, Eq (ParamKindFunctor k a))
  => Eq (OpEntry s k a) where
  E x == E y = x == y


-- Semigroup instance
instance Semigroup (OpDict '[] a) where
  Nil <> Nil = Nil -- The only possible equation

instance (Semigroup (OpEntry s k a), Semigroup (OpDict xs a))
  => Semigroup (OpDict ('(s, k) ': xs) a ) where
  (x :> xs) <> (y :> ys) = x <> y :> xs <> ys -- Also the only possible relation

instance (KnownSymbol s, KnownParamKind k, Semigroup (ParamKindFunctor k a))
  => Semigroup (OpEntry s k a) where
  (E x) <> (E y) = E (x <> y)

-- Monoid instance
instance Monoid (OpDict '[] a) where
  mempty = Nil

instance (Monoid (OpEntry s k a), Monoid (OpDict xs a))
  => Monoid (OpDict ('(s, k) ': xs) a ) where
  mempty = mempty :> mempty

instance (KnownSymbol s, KnownParamKind k, Monoid (ParamKindFunctor k a))
  => Monoid (OpEntry s k a) where
  mempty = E mempty


-- 'Default' instance
instance Default (OpDict '[] a) where
  def = Nil

instance (Default (OpEntry s k a), Default (OpDict xs a))
  => Default ( OpDict ('(s, k) ': xs) a) where
  def = def :> def

instance (Default (ParamKindFunctor k a))
  => Default (OpEntry s k a) where
  def = E def

-- 'ToQueryText' instance
instance ToQueryText (OpDict '[] a) where
  toQueryTextPrio _ _ = []

instance (ToQueryText (OpDict xs a), ToQueryText (OpEntry s k a))
  => ToQueryText (OpDict ('(s,k) ': xs) a) where
  toQueryTextPrio paramName (x :> xs) =
    toQueryTextPrio paramName x ++ toQueryTextPrio paramName xs

instance {-# OVERLAPPABLE #-} (KnownSymbol s, ToParamKindText k, ToHttpConstraint k a)
  => ToQueryText (OpEntry s k a) where
  toQueryTextPrio = toQueryTextPrioEntry Proxy id

instance {-# OVERLAPPING #-} (KnownSymbol s, ToParamKindText k, ToHttpConstraint k a, UntagMaybe (ParamKindFunctor k))
  => ToQueryText (OpEntry s k (Maybe a)) where
  toQueryTextPrio = toQueryTextPrioEntry Proxy untagMaybe

toQueryTextPrioEntry
  :: (KnownSymbol s, ToParamKindText k, ToHttpConstraint k a)
  => Proxy s
  -> (ParamKindFunctor k b -> ParamKindFunctor k a)
  -> Text
  -> OpEntry s k b
  -> QueryTextWithPrio
toQueryTextPrioEntry p f paramName (E xs) = attachName
  $ toParamKindText (Proxy :: Proxy k) (f xs)
 where
  attachName :: [Maybe Text] -> QueryTextWithPrio
  attachName = filterToQuery . map (FilterParam paramName opName)

  opName :: Text
  opName = Text.pack $ symbolVal p

-- | Removes a Just from the type
class UntagMaybe t where
  untagMaybe :: t (Maybe a) -> t a

instance UntagMaybe [] where
  untagMaybe = catMaybes

instance UntagMaybe Maybe where
  untagMaybe (Just (Just x)) = Just x
  untagMaybe _               = Nothing

instance UntagMaybe TaggedBool where
  untagMaybe Present    = Present
  untagMaybe NotPresent = NotPresent

-- 'FromQueryText' instance

instance FromQueryText (OpDict '[] a) where
  fromQueryText _ _ = Absent Nil -- I cannot prove the operator is there

instance (FromQueryText (OpDict xs a), FromQueryText (OpEntry s k a))
  => FromQueryText (OpDict ('(s,k) ': xs) a) where
  fromQueryText paramName qs =
    (:>) <$> fromQueryText paramName qs <*> fromQueryText paramName qs

instance {-# OVERLAPPABLE #-} (KnownSymbol s, FromParamKindText k, FromHttpConstraint k a)
  => FromQueryText (OpEntry s k a) where
  fromQueryText = fromQueryTextEntry Proxy id

instance {-# OVERLAPPING #-} (KnownSymbol s, FromParamKindText k
    , Functor (ParamKindFunctor k), FromHttpConstraint k a)
  => FromQueryText (OpEntry s k (Maybe a)) where
  fromQueryText = fromQueryTextEntry Proxy tagMaybe

-- | Helper for 'OpEntry' instances of 'FromQueryText'
fromQueryTextEntry
  :: (KnownSymbol s, FromParamKindText k, FromHttpConstraint k a)
  => Proxy s
  -> (ParamKindFunctor k a -> ParamKindFunctor k b)
  -> Text
  -> QueryText
  -> ParseResult (OpEntry s k b)
fromQueryTextEntry p f paramName qs =
  fmap (E . f) . parseParamKindText (Proxy :: Proxy k) $ filters
 where
  filters :: [Maybe Text]
  filters =
    map filterValue
      . findFilters paramName [Text.pack $ symbolVal p]
      . queryToFilter
      $ qs

-- | Adds a Just to the type
tagMaybe
  :: (Functor (ParamKindFunctor k))
  => ParamKindFunctor k a
  -> ParamKindFunctor k (Maybe a)
tagMaybe = fmap Just


-- | Like QueryParam but then with operators
data FilterParam = FilterParam
  { filterColumn :: Text -- ^ The column name with dots
  , filterOperator :: Text -- ^ Text between brackets
  , filterValue :: Maybe Text -- ^ thing after the equal sign
  } deriving (Eq, Show)

type FilterParams = [FilterParam]

-- | Extracts operator information from query parameters
-- Inverse to 'filterToQuery'
queryToFilter :: QueryText -> FilterParams
queryToFilter = map toFilter
 where
  toFilter (cs, val) = case parseOp bracketOp of
    Just op -> FilterParam col op val
    Nothing -> FilterParam cs Text.empty val
    where (col, bracketOp) = Text.span (/= '[') cs

  parseOp op = Text.stripPrefix "[" =<< Text.stripSuffix "]" op

-- | Extracts filter information of query parameters
-- Inverse to 'queryToFilter'
filterToQuery :: FilterParams -> QueryTextWithPrio
filterToQuery = map toQuery
 where
  toQuery (FilterParam col op val) =
    (0, if Text.null op then (col, val) else (col <> "[" <> op <> "]", val))

-- | Takes a ColumnName and a list of possible operators and test if the 'FilterParam'
-- matches these conditions
isFilterFor :: Text -> [Text] -> FilterParam -> Bool
isFilterFor col [] (FilterParam col' x _) =
  Text.toCaseFold col == Text.toCaseFold col' && Text.null x
isFilterFor col xs (FilterParam col' x _) =
  Text.toCaseFold col
    ==     Text.toCaseFold col'
    &&     Text.toCaseFold x
    `elem` map Text.toCaseFold xs

-- | Find all matching filters
findFilters :: Text -> [Text] -> FilterParams -> FilterParams
findFilters col xs = filter (isFilterFor col xs)

class FromParamKindText (k :: ParamKind) where
  parseParamKindText :: FromHttpConstraint k p => Proxy k -> [ Maybe Text ] -> ParseResult (ParamKindFunctor k p)

instance FromParamKindText 'List where
  parseParamKindText _ xs =
    let cxs = catMaybes xs
    in  case mapM parseQueryParam cxs of
          Right []  -> Absent []
          Right x   -> Found x
          Left  err -> ParseError err


instance FromParamKindText 'Normal where
  parseParamKindText _ xs =
    let mx = headMaybe . catMaybes $ xs
    in  case mapM parseQueryParam mx of
          Right Nothing  -> Absent (MaybeLast Nothing)
          Right (Just x) -> Found (MaybeLast (Just x))
          Left  err      -> ParseError err

instance FromParamKindText 'Flag where
  parseParamKindText _ xs =
    if null xs then Absent NotPresent else Found Present

class ToParamKindText (k :: ParamKind) where
  toParamKindText :: ToHttpConstraint k p => Proxy k -> ParamKindFunctor k p -> [ Maybe Text ]

instance ToParamKindText 'List where
  toParamKindText _ = map (Just . toQueryParam)

instance ToParamKindText 'Normal where
  toParamKindText _ (MaybeLast (Just x)) = [Just (toQueryParam x)]
  toParamKindText _ (MaybeLast Nothing ) = []

instance ToParamKindText 'Flag where
  toParamKindText _ Present    = [Nothing]
  toParamKindText _ NotPresent = []
