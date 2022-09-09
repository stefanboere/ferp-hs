{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: ProjectM36.Beamable.Expand
Description: Automatic foreign key joins

This module allows you to automatically include data from rows referenced by
foreign keys, /as if this data belongs to the original table/.

This allows you, for example, to include labels for foreign keys
in the response body and save some round trips to the server, and let users
filter and order by the /labels/. It has the same effect as manually creating
database views, but now completely typesafe and automatic.

== Foreign keys with labels
We now present an example where each record has a human readable label.

Consider a model like this.

> data UserT f = User
>   { _userId :: C f Int
>   , _userName :: C f Text
>   , _userAddress :: C AddressT f
>   } deriving (Generic, Beamable)
>
> data AddressT f = Address
>   { _addressId :: C f Int
>   , _addressStreet :: C f Text
>   } deriving (Generic, Beamable)

The address ids in the UserT table mean nothing to the end users, so we also need
to provide some readable description of the addresses. We want to include that in
the same request, as this saves another round trip to the server. Something like

@
GET \/users\/3
{ id : 3,
  name : "John Doe",
  address : { id = 5, name = "23 Jump Street" }
}
@

Here is how you can do that. Upgrade your user as follows

> data UserTT g f = { _userId :: C f Int
>                   , _userName :: C f Text
>                   , _userAddress :: D g AddressT f
>                   } deriving (Generic, Beamable1)
>
> -- Only the foreign key and nothing more, i.e. just what we had before
> type UserT f = UserTT PrimaryKey f
>
> -- The primary key and an label
> type UserN f = UserTT Named f
>
> -- So that we can query the database and get a 'UserT Identity' back
> deriving instance Beamable (UserT f)
>
> -- So that we can query the database and get a 'UserN Identity' back
> deriving instance Beamable (UserN f)
> ...

Now 'UserN' is just 'UserT', but with the primary keys replaced by objects
consisting of the primary key and the display name:

> john :: UserN Identity
> john = User { _userId = 3
>             , _userName = "John Doe"
>             , _userAddress = Named { _id = AddressId 5, _name = "23 Jump Street" }
>             }

Then you can use 'all_'' to automatically query the database with these types.
For this to work, we must specify what the label of the address is. This is done by
creating an instance of 'ToName':

> instance ToName AddressT where
>   toName = _addressStreet
-}
module ProjectM36.Beamable.Expand
  ( D
  , D'(..)
  , Referenced
  , Beamable1
  , IsView(..)
    -- * Types to apply to 'D'
  , Named(..)
  , WithName(..)
  , ToName(..)
  , Full
  , BaseTable
    -- * Querying functions
  , vwFromTuple
  , vwRelationalExpr
  , flattenNamed
  , flattenFull
    -- * Other
  , TablesFulfillConstraint
  , HasNameConstraint
  ) where

import           Prelude

import           Control.Monad.Writer           ( Endo(..)
                                                , Writer
                                                , execWriter
                                                , tell
                                                )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Kind                      ( Constraint
                                                , Type
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           GHC.Generics            hiding ( C
                                                , D
                                                )
import           ProjectM36.Atomable            ( Atomable )
import           ProjectM36.Base
import           ProjectM36.Beamable.Class
import           ProjectM36.Beamable.Tupleable
import           ProjectM36.Error
import           ProjectM36.Tuple               ( atomForAttributeName )

-- | A newtype storing the type data passed to 'D'. Analogous to 'Columnar''.
newtype D' p t f = D' (D p t f)

-- | Replacement rules for Primary key types.
--
-- The basic rules are as follows
--
-- > D PrimaryKey t f = PrimaryKey t f
--
-- Hence 'PrimaryKey' is like the 'Identity' for 'Columnar'.
--
-- You can attach a name to primary keys using 'Named':
--
-- > D Named t f = Named t f
--
-- It is also possible to include the entire object:
--
-- > D Full t f = t f
--
-- Finally use 'Referenced' to create an object which stores which tables
-- to use in the automatic join queries.
--
-- > D Referenced t f = TableEntity t
--
-- This allows you to enter this data as follows
--
-- > useraddressJoin :: UserTT (Referenced Postgres AppDatabase) Proxy
-- > useraddressJoin = User Proxy Proxy (appDatabase ^. appDatabaseAddresses)
-- brittany-disable-next-binding
type family D p (t :: (Type -> Type) -> Type) (f :: (Type -> Type)) where
  D PrimaryKey t f = PrimaryKey (BaseTable t) f
  D Named t f = Named (BaseTable t) f
  D Full t f = t f
  D Referenced t f = PrimaryKey (BaseTable t) (TableField (BaseTable t))
  D (WithConstraint1 c) t f = WithConstraint1 c t f
  D Exposed1 t f = Exposed1 t f
  D a t f = a t f

-- | Pass this type to 'D' to include the full referenced object. See 'D'.
-- brittany-disable-next-binding
data Full (t :: (Type -> Type) -> Type) (f :: (Type -> Type))

-- | Use this to create an object storing the table entities of the foreign keys.
-- brittany-disable-next-binding
data Referenced (t :: (Type -> Type) -> Type) (f :: (Type -> Type))

-- | Internal used type to extract the meta data
-- brittany-disable-next-binding
data Exposed1 (t :: (Type -> Type) -> Type) (f :: (Type -> Type))

-- | A 'Named table f' consist of the '_id', which is of type 'PrimaryKey table f'
-- and a human readable description of type text ( 'C f Text')
-- brittany-disable-next-binding
data Named (t :: (Type -> Type) -> Type) (f :: (Type -> Type)) =
    Named
      { _id :: PrimaryKey t f
      , _name :: C f Text
      } deriving stock (Generic)

-- | 'Named' is instance of beamable, so you can actually use regular filters,
-- orderings, etc, as you would normally do for Embedded types.
-- See the example of the AddressMixing in https://tathougies.github.io/beam/user-guide/models/
-- for how useful this actually is.
-- The upshot is that the text can be queried as if it was a field of the @t@ table.
instance Beamable (PrimaryKey t) => Beamable (Named t)

instance (Eq (PrimaryKey t f), Eq (C f Text)) => Eq (Named t f) where
  (Named i1 n1) == (Named i2 n2) = i1 == i2 && n1 == n2

instance (Show (PrimaryKey t f), Show (C f Text)) => Show (Named t f) where
  show (Named i n) =
    "Named { _id = " <> show i <> " , _name = " <> show n <> " }"

instance (Semigroup (PrimaryKey t f), Semigroup (C f Text))
  => Semigroup (Named t f) where
  Named i0 n0 <> Named i1 n1 = Named (i0 <> i1) (n0 <> n1)

instance (Monoid (PrimaryKey t f), Monoid (C f Text))
  => Monoid (Named t f) where
  mempty = Named mempty mempty

-- | We need to know how to create a label for a record.
-- This should only be done once for each type, hence we implemented it as a typeclass.
-- For each type you want to add a label (so this will practically be almost every record type
-- in your schema), you need to create an instance of this class.
--
-- > instance ToName UserT where
-- >   name = _userName
class Table t => ToName t where
  -- | Extract the id and display name from a table 't'
  toName :: Proxy t -> AtomExpr

-- | The record together with the label
-- brittany-disable-next-binding
data WithName (t :: (Type -> Type) -> Type) (f :: (Type -> Type)) =
    WithName
      { _wn_data :: t f
      , _wn_name :: C f Text
      } deriving stock (Generic)

instance Beamable t => Beamable (WithName t)

instance (Eq (t f), Eq (C f Text)) => Eq (WithName t f) where
  (WithName i1 n1) == (WithName i2 n2) = i1 == i2 && n1 == n2

instance (Show (t f), Show (C f Text)) => Show (WithName t f) where
  show (WithName i n) =
    "WithName { _wn_data = " <> show i <> " , _wn_name = " <> show n <> " }"

instance (Semigroup (t f), Semigroup (C f Text))
  => Semigroup (WithName t f) where
  WithName i0 n0 <> WithName i1 n1 = WithName (i0 <> i1) (n0 <> n1)

instance (Monoid (t f), Monoid (C f Text))
  => Monoid (WithName t f) where
  mempty = WithName mempty mempty

type family BaseTable (t :: (Type -> Type) -> Type) :: (Type -> Type) -> Type
type instance BaseTable (WithName t) = BaseTable t

class IsView (n :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type) (t :: (Type -> Type) -> Type) where
  dbRelationalExpr :: Proxy n -> Proxy t -> RelationalExpr
  dbFromTuple :: Proxy n -> Proxy t -> RelationTuple -> Either RelationalError (D n t Identity)

instance {-# OVERLAPPABLE #-} (Table t, FieldsFulfillConstraint Atomable t)
  => IsView Full t where
  dbRelationalExpr _ p =
    Restrict (dbFilterExpr p) (RelationVariable (tblEntityName p) ())
  dbFromTuple _ _ = tblFromTuple

instance (Table (BaseTable t), FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t)), IsView Full t
     , ToName t) => IsView Named t where
  dbRelationalExpr _ t =
    Project (AttributeNames (Set.insert "name" $ primaryKeyAttributeNames t))
      $ Extend (AttributeExtendTupleExpr "name" (toName t))
               (dbRelationalExpr (Proxy :: Proxy Full) t)

  dbFromTuple _ _ tupl = Named <$> tblFromTuplePk tupl <*> readName tupl

readName :: RelationTuple -> Either RelationalError Text
readName tupl = do
  nameAtom <- atomForAttributeName "name" tupl
  case nameAtom of
    TextAtom t -> Right t
    _          -> Left $ AtomTypeMismatchError TextAtomType TextAtomType

instance (IsView Full t, ToName t) => IsView WithName t where
  dbRelationalExpr _ t = Extend (AttributeExtendTupleExpr "name" (toName t))
                                (dbRelationalExpr (Proxy :: Proxy Full) t)
  dbFromTuple _ t tupl =
    WithName <$> dbFromTuple (Proxy :: Proxy Full) t tupl <*> readName tupl

instance (Beamable1 t, Table (t PrimaryKey)
     , IsView Full (t PrimaryKey)
     , TablesFulfillConstraint Proxy (ToExpanded' Named) t
     , TablesFulfillConstraint Identity (ToExpanded' Named) t
     , FieldsFulfillConstraint Atomable (t PrimaryKey)
     , TablesFulfillConstraint (TableField (t PrimaryKey)) BeamablePk t
         ) => IsView Full (t Named) where
  dbRelationalExpr = vwRelationalExpr
  dbFromTuple      = vwFromTuple

instance (Beamable1 t, Table (t PrimaryKey)
     , IsView Full (t PrimaryKey)
     , TablesFulfillConstraint Proxy (ToExpanded' Full) t
     , TablesFulfillConstraint Identity (ToExpanded' Full) t
     , FieldsFulfillConstraint Atomable (t PrimaryKey)
     , TablesFulfillConstraint (TableField (t PrimaryKey)) BeamablePk t
         ) => IsView Full (t Full) where
  dbRelationalExpr = vwRelationalExpr
  dbFromTuple      = vwFromTuple

instance (Beamable1 t, Table (t PrimaryKey)
     , IsView Full (t PrimaryKey)
     , TablesFulfillConstraint Proxy (ToExpanded' WithName) t
     , TablesFulfillConstraint Identity (ToExpanded' WithName) t
     , FieldsFulfillConstraint Atomable (t PrimaryKey)
     , TablesFulfillConstraint (TableField (t PrimaryKey)) BeamablePk t
         ) => IsView Full (t WithName) where
  dbRelationalExpr = vwRelationalExpr
  dbFromTuple      = vwFromTuple

instance (t ~ BaseTable t, IsView Named t) => IsView Full (Named t) where
  dbRelationalExpr _ _ =
    dbRelationalExpr (Proxy :: Proxy Named) (Proxy :: Proxy t)
  dbFromTuple _ _ = dbFromTuple (Proxy :: Proxy Named) (Proxy :: Proxy t)

instance IsView WithName t => IsView Full (WithName t) where
  dbRelationalExpr _ _ =
    dbRelationalExpr (Proxy :: Proxy WithName) (Proxy :: Proxy t)
  dbFromTuple _ _ = dbFromTuple (Proxy :: Proxy WithName) (Proxy :: Proxy t)

-- | This is 'Beamable' for the enhanced records. This makes sure we can zip only
-- over the foreign key fields.
--
-- Types which normally would derive 'Beamable', will now derive 'Beamable1'.
-- It only makes sense to use this for types which contains Foreign Keys, otherwise
-- there is nothing to expand.
class Beamable1 (table :: (((Type -> Type) -> Type) -> (Type -> Type) -> Type) -> (Type -> Type) -> Type)  where
    -- | Zips the foreign key fields, applying the first function to each of the
    -- foreign key fields and returning the right hand side for other, non foreign key, fields
    zipBeamFieldsM1
      :: Applicative m
      => (forall t. D' f t b -> D' g t a -> m (D' h t a))
      -> table f b
      -> table g a
      -> m (table h a)

    default zipBeamFieldsM1
      :: ( HasBeamFields1 table f g h a b
         , Applicative m
         )
      => (forall t. D' f t b -> D' g t a -> m (D' h t a))
      -> table f b
      -> table g a
      -> m (table h a)

    zipBeamFieldsM1 combine f (g :: table f a) =
      to <$> gZipTables1 (Proxy :: Proxy (Rep (table Exposed1 Proxy))) combine (from f) (from g)

-- | Constraint on the type to be able to zip it.
-- brittany-disable-next-binding
type HasBeamFields1 (table ::  (((Type -> Type) -> Type) -> (Type -> Type) -> Type) -> (Type -> Type) -> Type) f g h a b
  = ( GZipTables1
      f
      g
      h
      a
      b
      (Rep (table Exposed1 Proxy))
      (Rep (table f b))
      (Rep (table g a))
      (Rep (table h a))
    , Generic (table f b)
    , Generic (table g a)
    , Generic (table h a)
    )

-- | Generic implementation for  zipBeamFieldsM1' of 'Beamable1'
class GZipTables1 f g h a b (exposedRep :: Type -> Type) fRep gRep hRep where
    gZipTables1
      :: Applicative m
      => Proxy exposedRep
      -> (forall t. D' f t b -> D' g t a -> m (D' h t a))
      -> fRep ()
      -> gRep ()
      -> m (hRep ())

instance ( GZipTables1 f g h a b exp1 f1 g1 h1
         , GZipTables1 f g h a b exp2 f2 g2 h2
         )
  => GZipTables1 f g h a b (exp1 :*: exp2) (f1 :*: f2) (g1 :*: g2) (h1 :*: h2) where

  gZipTables1 _ combine ~(f1 :*: f2) ~(g1 :*: g2) =
    (:*:)
      <$> gZipTables1 (Proxy :: Proxy exp1) combine f1 g1
      <*> gZipTables1 (Proxy :: Proxy exp2) combine f2 g2


instance GZipTables1 f g h a b exp fRep gRep hRep =>
    GZipTables1 f g h a b (M1 x y exp) (M1 x y fRep) (M1 x y gRep) (M1 x y hRep) where
  gZipTables1 _ combine ~(M1 f) ~(M1 g) =
    M1 <$> gZipTables1 (Proxy :: Proxy exp) combine f g

-- | Here we specify what to do for foreign key fields
instance ( fa ~ D f t b
         , ga ~ D g t a
         , ha ~ D h t a) =>
    GZipTables1 f g h a b (K1 R (Exposed1 t Proxy)) (K1 R fa) (K1 R ga) (K1 R ha) where
  gZipTables1 _ combine ~(K1 f) ~(K1 g) =
    (\(D' h) -> K1 h) <$> combine (D' f :: D' f t b) (D' g :: D' g t a)

-- | Here we specify what to do for the non-foreign key fields (just return the right hand side)
instance ( fa ~ C b c
         , ga ~ C a c
         , ha ~ C a c) =>
    GZipTables1 f g h a b (K1 R (Proxy c)) (K1 R fa) (K1 R ga) (K1 R ha) where
  gZipTables1 _ _ _ ~(K1 g) = pure (K1 g)

-- | This is for nested types
instance ( Beamable1 tbl
         ) => GZipTables1 f g h a b (K1 R (tbl Exposed1 Proxy)) (K1 R (tbl f b))
                                                          (K1 R (tbl g a))
                                                          (K1 R (tbl h a))
   where
  gZipTables1 _ combine ~(K1 f) ~(K1 g) = K1 <$> zipBeamFieldsM1 combine f g

instance GZipTables1 f g h a b U1 U1 U1 U1 where
  gZipTables1 _ _ _ _ = pure U1

-- | Tags a primary key field with a constraint
data WithConstraint1 (c :: ((Type -> Type) -> Type) -> Constraint ) f a where
  WithConstraint1 ::c f => D PrimaryKey f a -> WithConstraint1 c f a

-- | Assert that all foreign key fields in the table satisfy a constraint
-- brittany-disable-next-binding
type TablesFulfillConstraint f (c :: ((Type -> Type) -> Type) -> Constraint) (t ::(((Type -> Type) -> Type) -> (Type -> Type) -> Type) -> (Type -> Type) -> Type) =
  ( Generic (t (WithConstraint1 c) f), Generic (t PrimaryKey f), Generic (t Exposed1 Proxy)
  , GTablesFulfillConstraint c (Rep (t Exposed1 Proxy)) (Rep (t PrimaryKey f)) (Rep (t (WithConstraint1 c) f))
  )

-- | Generic check if all fields satisfy a constraint
-- brittany-disable-next-binding
class GTablesFulfillConstraint (c :: ((Type -> Type) -> Type) -> Constraint) (exposed :: Type -> Type) values withconstraint where
  gWithConstrainedTables :: Proxy c -> Proxy exposed -> values () -> withconstraint ()

-- brittany-disable-next-binding
instance GTablesFulfillConstraint c exposed values withconstraint =>
    GTablesFulfillConstraint c (M1 s m exposed) (M1 s m values) (M1 s m withconstraint) where
  gWithConstrainedTables c _ (M1 x) =
    M1 (gWithConstrainedTables c (Proxy @exposed) x)

instance GTablesFulfillConstraint c U1 U1 U1 where
  gWithConstrainedTables _ _ _ = U1

-- brittany-disable-next-binding
instance (GTablesFulfillConstraint c aExp a aC, GTablesFulfillConstraint c bExp b bC) =>
  GTablesFulfillConstraint c (aExp :*: bExp) (a :*: b) (aC :*: bC) where
  gWithConstrainedTables be _ (a :*: b) =
    gWithConstrainedTables be (Proxy @aExp) a
      :*: gWithConstrainedTables be (Proxy @bExp) b

instance (c t, t0 ~ BaseTable t) => GTablesFulfillConstraint c (K1 R (Exposed1 t Proxy)) (K1 R (PrimaryKey t0 f)) (K1 R (WithConstraint1 c t f)) where
  gWithConstrainedTables _ _ (K1 x) = K1 (WithConstraint1 x)

instance GTablesFulfillConstraint c (K1 R (Proxy b)) (K1 R (f b)) (K1 R (f b)) where
  gWithConstrainedTables _ _ (K1 x) = K1 x

instance GTablesFulfillConstraint c (K1 R (Proxy b)) (K1 R b) (K1 R b) where
  gWithConstrainedTables _ _ (K1 x) = K1 x

-- brittany-disable-next-binding
instance TablesFulfillConstraint f c t =>
    GTablesFulfillConstraint c (K1 R (t Exposed1 Proxy)) (K1 R (t PrimaryKey f)) (K1 R (t (WithConstraint1 c) f)) where
  gWithConstrainedTables _ _ (K1 x) = K1
    (to
      (gWithConstrainedTables (Proxy @c)
                              (Proxy @(Rep (t Exposed1 Proxy)))
                              (from x)
      )
    )

type HasTableEquality tbl = (FieldsFulfillConstraint Eq tbl, Beamable tbl)

-- | A field has the right constraints to be used in 'expand'.
class (Table t, FieldsFulfillConstraint Atomable t, HasTableEquality (PrimaryKey t), IsView f t) => HasNameConstraint f t
instance (Table t, FieldsFulfillConstraint Atomable t, HasTableEquality (PrimaryKey t), IsView f t) => HasNameConstraint f t

-- | Adds a constraint to each primary key field of the table.
-- brittany-disable-next-binding
withConstrainedTables
  :: forall c tbl f
   . TablesFulfillConstraint f c tbl
  => tbl PrimaryKey f
  -> tbl (WithConstraint1 c) f
withConstrainedTables =
  to
    . gWithConstrainedTables (Proxy @c) (Proxy @(Rep (tbl Exposed1 Proxy)))
    . from

-- | Removes the labels of the Named type, so that we have a type conversion
-- @tt Named g@@ to @@t PrimaryKey g@@
flattenNamed
  :: forall table f
   . (Beamable1 table, Beamable (table PrimaryKey))
  => table Named f
  -> table PrimaryKey f
flattenNamed n = runIdentity $ zipBeamFieldsM1
  (\_ (D' x) -> Identity $ D' (_id x))
  (tblSkeleton :: table PrimaryKey Proxy)
  n

class HasPrimaryKey t where
  getPrimaryKey :: t f -> PrimaryKey (BaseTable t) f

instance (Table t, BaseTable t ~ t) => HasPrimaryKey t where
  getPrimaryKey = primaryKey

-- | Remove expanded extra data and only keep the primaryKey. See @flattenNamed@
flattenFull
  :: forall table f
   . ( Beamable1 table
     , Beamable (table PrimaryKey)
     , TablesFulfillConstraint Proxy HasPrimaryKey table
     )
  => table Full f
  -> table PrimaryKey f
flattenFull n = runIdentity $ zipBeamFieldsM1
  combine
  (withConstrainedTables (tblSkeleton :: table PrimaryKey Proxy))
  n
 where
  combine
    :: D' (WithConstraint1 HasPrimaryKey) t Proxy
    -> D' Full t f
    -> Identity (D' PrimaryKey t f)
  combine (D' (WithConstraint1 c)) (D' x) = Identity $ D' $ go c x

  go
    :: HasPrimaryKey t
    => PrimaryKey (BaseTable t) Proxy
    -> t f
    -> PrimaryKey (BaseTable t) f
  go _ = getPrimaryKey

class Beamable (PrimaryKey (BaseTable t)) => BeamablePk t
instance Beamable (PrimaryKey (BaseTable t)) => BeamablePk t

class (IsView n t, Table (BaseTable t), Beamable (PrimaryKey (BaseTable t))) => ToExpanded' n t
instance (IsView n t, Table (BaseTable t), Beamable (PrimaryKey (BaseTable t))) => ToExpanded' n t

vwRelationalExpr
  :: forall t n
   . ( Beamable1 t
     , Table (t PrimaryKey)
     , IsView Full (t PrimaryKey)
     , TablesFulfillConstraint Proxy (ToExpanded' n) t
     , TablesFulfillConstraint (TableField (t PrimaryKey)) BeamablePk t
     )
  => Proxy Full
  -> Proxy (t n)
  -> RelationalExpr
vwRelationalExpr pf _ =
  let go = appEndo . execWriter $ zipBeamFieldsM1
        (doJoin (Proxy :: Proxy n))
        (refFieldSettings (Proxy :: Proxy t))
        (withConstrainedTables tblSkeleton)
  in  go (dbRelationalExpr pf (Proxy :: Proxy (t PrimaryKey)))
 where
  doJoin
    :: forall n' f
     . Proxy n'
    -> D' Referenced f Proxy
    -> D' (WithConstraint1 (ToExpanded' n')) f Proxy
    -> Writer (Endo RelationalExpr) (D' PrimaryKey f Proxy)
  doJoin p (D' col) (D' (WithConstraint1 x)) = do
    let renamer = appEndo $ columnNameMapping
          (\a b -> Endo (Rename a b))
          col
          (primaryKey (tblFieldSettings :: TableSettings (BaseTable f)))
    tell $ Endo $ Join (renamer (dbRelationalExpr p (Proxy :: Proxy f)))
    pure $ D' x

refFieldSettings
  :: forall table
   . ( Beamable1 table
     , Table (table PrimaryKey)
     , TablesFulfillConstraint
         (TableField (table PrimaryKey))
         BeamablePk
         table
     )
  => Proxy table
  -> table Referenced Proxy
refFieldSettings _ = runIdentity
  $ zipBeamFieldsM1 f (withConstrainedTables tblFieldSettings) tblSkeleton
 where
  f
    :: D' (WithConstraint1 BeamablePk) t (TableField (table PrimaryKey))
    -> D' PrimaryKey t Proxy
    -> Identity (D' Referenced t Proxy)
  f (D' (WithConstraint1 col)) _ = Identity
    $ D' (runIdentity $ zipBeamFieldsM coerceSettings col tblSkeleton)

  coerceSettings
    :: Columnar' (TableField (table PrimaryKey)) a
    -> b
    -> Identity (Columnar' (TableField t) a)
  coerceSettings (Columnar' (TableField x y)) _ =
    Identity $ Columnar' $ TableField x y

vwFromTuple
  :: forall table n
   . ( Beamable1 table
     , Table (table PrimaryKey)
     , TablesFulfillConstraint Identity (ToExpanded' n) table
     , FieldsFulfillConstraint Atomable (table PrimaryKey)
     , TablesFulfillConstraint
         (TableField (table PrimaryKey))
         BeamablePk
         table
     )
  => Proxy Full
  -> Proxy (table n)
  -> RelationTuple
  -> Either RelationalError (table n Identity)
vwFromTuple _ _ tupl = do
  x <- tblFromTuple tupl
  expandTuple' x tupl

-- | Read the non base columns from the env
expandTuple'
  :: forall table n
   . ( Beamable1 table
     , Table (table PrimaryKey)
     , TablesFulfillConstraint Identity (ToExpanded' n) table
     , TablesFulfillConstraint
         (TableField (table PrimaryKey))
         BeamablePk
         table
     )
  => table PrimaryKey Identity
  -> RelationTuple
  -> Either RelationalError (table n Identity)
expandTuple' tbl tupl = zipBeamFieldsM1
  (combine (Proxy :: Proxy n))
  (refFieldSettings (Proxy :: Proxy table))
  (withConstrainedTables tbl)
 where
  combine
    :: forall t
     . Proxy n
    -> D' Referenced t Proxy
    -> D' (WithConstraint1 (ToExpanded' n)) t Identity
    -> Either RelationalError (D' n t Identity)
  combine p (D' col) (D' (WithConstraint1 _)) = D' <$> dbFromTuple
    p
    (Proxy :: Proxy t)
    (renameCols
      (columnNameMapping
        Map.singleton
        (primaryKey (tblFieldSettings :: TableSettings (BaseTable t)))
        col
      )
      tupl
    )

  renameCols
    :: Map AttributeName AttributeName -> RelationTuple -> RelationTuple
  renameCols m (RelationTuple (Attributes attrs) v) = RelationTuple
    (Attributes (fmap r attrs))
    v
    where r (Attribute x t) = Attribute (Map.findWithDefault x x m) t

columnNameMapping
  :: forall m t
   . (Beamable (PrimaryKey t), Monoid m)
  => (AttributeName -> AttributeName -> m)
  -> PrimaryKey t (TableField t)
  -> PrimaryKey t (TableField t)
  -> m
columnNameMapping f orig renamed = execWriter
  $ zipBeamFieldsM mkRenamer orig renamed
 where

  mkRenamer
    :: Columnar' (TableField t) a
    -> Columnar' (TableField t) a
    -> Writer m (Columnar' Proxy a)
  mkRenamer (Columnar' o) (Columnar' r) = do
    tell $ f (fieldAttributeName r) (fieldAttributeName o)
    pure $ Columnar' Proxy
