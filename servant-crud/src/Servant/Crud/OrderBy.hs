{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module: Servant.Crud.OrderBy
Description: Type safe ordering combinators.

This module provides a method to parse a sort query parameter and obtain
ordering information together with record accessors.

This might be useful for converting it to your favourite database EDSL in a type safe way,
i.e. without passing strings around. The @beam-crud@ package provides an example of this.
-}
module Servant.Crud.OrderBy
  ( OrderBy
  , orderByDirection
  , orderBySelector
  , fromSelector
  , extract
  , Direction(..)
  , Path
  , dump
  , showFieldIn
  -- * Selectors
  , Selectors(..)
  , HSelector(..)
  , leaf
  , EQ
  , allValues
  , defaultSelectors
  , GSelectors
  )
where

import           Prelude

import           Data.Default                   ( Default(..) )
import           Data.Functor.Contravariant     ( Contravariant(..) )
import           Data.Kind                      ( Constraint )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics
import           GHC.TypeLits                   ( TypeError
                                                , ErrorMessage(..)
                                                )
import           Servant.API                    ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )

import           Servant.Crud.QueryObject       ( Options(..)
                                                , defaultOptions
                                                )


-- | Result of parsing a query parameter
data OrderBy (c :: * -> Constraint) r = OrderBy
  { _orderByPath :: Path -- ^ The list of path segments specified in the query parameter, used for the ToHttpApiData instance.
  , orderByDirection :: Direction -- ^ If the user wants lowest or highest first
  , orderBySelector :: HSelector c r -- ^ Accesser of a field in the record @r@
  }

-- | The path and direction, together with the type is unique
instance Eq (OrderBy c r) where
  (OrderBy p d _) == (OrderBy p' d' _) = p == p' && d == d'

instance Show (OrderBy c r) where
  show (OrderBy p d _) =
    "OrderBy { _orderByPath = "
      <> show p
      <> ", orderByDirection = "
      <> show d
      <> " }"

-- | If all inner types are of type @a@, we can extract a value from @r@ using
-- a function of type @Direction -> a -> b@.
extract :: (Direction -> a -> b) -> r -> OrderBy (EQ a) r -> b
extract f r (OrderBy _ dir (HSelector sel)) = f dir (sel r)

-- | Construct a OrderBy instance from the result of 'selectors' and a direction
fromSelector :: (Path, HSelector c r) -> Direction -> OrderBy c r
fromSelector (path, sel) ord = OrderBy path ord sel

-- | Create a string for debugging purposes
dump :: forall r . r -> OrderBy Show r -> String
dump x (OrderBy path dir (HSelector f)) =
  show dir <> ", " <> show path <> ", " <> show (f x)

-- | Inspect a field of @r@ with an OrderBy object
showFieldIn :: forall r . r -> OrderBy Show r -> String
showFieldIn x (OrderBy _ _ (HSelector f)) = show (f x)

-- | Either lowest first or hightest first
data Direction = Ascending | Descending deriving (Show, Eq, Enum, Bounded)

instance Default Direction where
  def = Ascending

-- | Parsed ordering info
data OrderingParam = OrderingParam
  { orderingDirection :: Direction -- ^ Direction to sort the column
  , orderingColumn :: Text -- ^ Which column to sort
  }
  deriving (Show, Eq)

-- | Parse a column name with plusses or minusses to 'OrderingParams'
parseFragment :: Text -> Either Text OrderingParam
parseFragment t = case Text.uncons . normalize $ t of
  Just (x, xs) -> case x of
    '+' -> Right $ OrderingParam Ascending (Text.strip xs)
    '-' -> Right $ OrderingParam Descending (Text.strip xs)
    _   -> Right $ OrderingParam Ascending (Text.cons x xs)
  Nothing -> Left "Empty column name"

-- | A list of path segments
type Path = [Text]

-- | This means it can be used in 'QueryParam' and 'QueryParams'
--
-- The query parameter may start with a @+@ or @-@
-- indicating resp. Ascending or Descending ordering.
--
-- The rest of the query parameter is interpreted as a list of Path fragments, dot separated.
-- See 'Selectors' how this list is interpreted.
--
-- @?sort=-user.name@
instance Selectors c r => FromHttpApiData (OrderBy c r) where
  parseQueryParam param = do
    p <- parseFragment param
    let path = Text.split (== '.') (orderingColumn p)
    sel <- maybe (Left $ "Invalid column name " <> param)
                 Right
                 (lookup path sels)
    pure $ OrderBy path (orderingDirection p) sel
   where
    sels :: [(Path, HSelector c r)]
    sels = selectors

instance ToHttpApiData (OrderBy c r) where
  toQueryParam (OrderBy path Ascending  _) = Text.intercalate "." path
  toQueryParam (OrderBy path Descending _) = "-" <> Text.intercalate "." path

-- UTILITIES
addPrefix :: String -> Path -> Path
addPrefix pref xs = if Text.null tpref then xs else tpref : xs
  where tpref = normalize . Text.pack $ pref

-- | Drop the constructor name
addCPrefix :: Constructor c => C1 c f () -> Options -> Path -> Path
addCPrefix c1 opts = addPrefix (constructorTagModifier opts (conName c1))

-- | Drop the selector name
addSPrefix :: Selector s => S1 s f () -> Options -> Path -> Path
addSPrefix s1 opts = addPrefix (fieldLabelModifier opts (selName s1))

mapBoth :: (a -> a1) -> (b -> b1) -> (a, b) -> (a1, b1)
mapBoth f g (x, y) = (f x, g y)

normalize :: Text -> Text
normalize = Text.toCaseFold . Text.strip

-- | Heterogeneous record selector
--
-- Just a record selector @r -> b@ for some type @b@, but with an additional
-- constraint enforced so that we can actually do something with it.
--
-- This is mostly for library autors who want to convert an 'OrderBy' type to there
-- database EDSL.
--
-- > data Foo= Foo { unFoo :: String, unBar :: String } deriving (Generic)
--
-- As all types are strings, we should just be able to convert this into a list of Strings, right?
-- Indeed, first specify when to stop the recursion to inner types
--
-- > class a ~ String => EqString a -- If @a@ is String, then we call this @EqString@
-- > instance EqString String -- The @String@ type is equal to the @String@ type
-- > instance Selectors EqString String where
-- >   selectors = leaf -- Stop if we encounter the @String@ type
--
-- Then tell the compiler that we know that all types in @Foo@ are strings
--
-- > instance Selectors IsString Foo
--
-- Finally create a list as follows
--
-- > allStringValues :: forall r . Selectors EqString r => r -> [String]
-- > allStringValues x = fmap (toString . snd) selectors
-- >  where
-- >   toString :: HSelector EqString r -> String
-- >   toString (HSelector sel) = fromString (sel x)
--
-- >>> foo = Foo "Hello" "Haskell"
-- >>> allStringValues foo
-- ["Hello","Haskell"]
data HSelector (c :: * -> Constraint) r = forall b . (c b) => HSelector (r -> b)

-- | Specify that this is an inner most type and one does not need to go deeper
leaf :: (c r) => [(Path, HSelector c r)]
leaf = [([], HSelector id)]

instance Contravariant (HSelector c) where
  contramap f (HSelector g) = HSelector (g . f)

-- | Synonym for '~', because this is also the lazyness annotation
class (a ~ b) => EQ a b
instance (a ~ b) => EQ a b

-- | From the type @a@ we can extract a value of type @a@.
instance Selectors (EQ a) a where
  selectors = leaf -- Stop if we encounter the @a@ type

-- | Once you have a proof that all value of your record @r@ are of type @a@ ('Selectors (EQ a) r')
-- you can create a list of all the values.
allValues :: forall r a . Selectors (EQ a) r => r -> [a]
allValues x = fmap (toVal . snd) selectors
 where
  toVal :: HSelector (EQ a) r -> a
  toVal (HSelector sel) = sel x

-- | Key value pair of names and accessors, with some constraints
class Selectors (c :: * -> Constraint) r where
  selectors :: [(Path, HSelector c r)]
  default selectors :: (Generic r, GSelectors c (Rep r)) => [(Path, HSelector c r)]
  selectors = defaultSelectors defaultOptions

-- | Generic implementation of 'selectors'
--
-- Constructor names and Selector names which are encountered on the way to the inner most type
-- are appended to the 'Path'. This behaviour can be modified with 'Options'.
defaultSelectors
  :: (Generic r, GSelectors c (Rep r)) => Options -> [(Path, HSelector c r)]
defaultSelectors = fmap (fmap (contramap from)) . gSelectors

-- | GStrSelector for types of kind (* -> *)
class GSelectors (c :: * -> Constraint) (a :: * -> *) where
  gSelectors :: Options -> [(Path, HSelector c (a p))]

-- | Parse unary data types
instance {-# OVERLAPPING #-} (GSelectors c f, Constructor k)
  => GSelectors c (D1 t (C1 k f)) where
  gSelectors opts = fmap prefix (gSelectors opts)
   where
    prefix = mapBoth f1 f2
    f1     = if unwrapUnaryConstructors opts
      then id
      else addCPrefix (undefined :: C1 k f ()) opts
    f2 = contramap (unM1 . unM1)

-- | Add the constructor name to the list of path segments for all other constructors
instance (GSelectors c f, Constructor k)
  => GSelectors c (C1 k f) where
  gSelectors opts = fmap prefix (gSelectors opts)
   where
    prefix = mapBoth f1 f2
    f1     = addCPrefix (undefined :: C1 k f ()) opts
    f2     = contramap unM1

-- | Add selector to the list of prefixes
instance (GSelectors c f, Selector s)
  => GSelectors c (S1 s f) where
  gSelectors opts = fmap prefix (gSelectors opts)
   where
    prefix = mapBoth f1 f2
    f1     = addSPrefix (undefined :: S1 s f ()) opts
    f2     = contramap unM1

-- | For products we just need both
instance (GSelectors c f, GSelectors c g)
  => GSelectors c (f :*: g) where
  gSelectors opts = fmap (fmap (contramap p1)) (gSelectors opts)
    ++ fmap (fmap (contramap p2)) (gSelectors opts)
   where
    p1 (x :*: _) = x
    p2 (_ :*: y) = y

-- | For sums we return the first one which succeeds
instance TypeError ('Text "There is no Selectors instance for sum types, as this would result in a partial function.")
  => GSelectors c (f :+: g) where
  gSelectors = error "unreachable"

-- | This is a wrapper for the inner most datatype. So we just apply the fromQueryText instance
instance (Selectors c k) => GSelectors c (K1 i k) where
  gSelectors _ = fmap (fmap (contramap unK1)) selectors

-- | The inner most wrapper might also be a Unit
instance GSelectors c U1 where
  gSelectors _ = []
