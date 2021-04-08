{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Database.Beam.Expand
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
module Database.Beam.Expand
  ( all_'
  , D
  , D'(..)
  , Referenced
  , Beamable1
    -- * Types to apply to 'D'
  , Named(..)
  , ToName(..)
  , Full
  , ToExpanded(..)
    -- * Querying functions
  , expand
    -- * Other
  , TablesFulfillConstraint
  , HasNameConstraint
  )
where

import           Prelude

import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                , object
                                                , withObject
                                                )
import           Data.Kind                      ( Constraint )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Database.Beam
import           Database.Beam.Backend.SQL      ( BeamSqlBackend )
import           Database.Beam.Schema.Tables    ( Ignored )
import           GHC.Generics            hiding ( C
                                                , D
                                                )

import           Servant.Crud.QueryObject       ( FromQueryText(..)
                                                , ToQueryText(..)
                                                )

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
-- Finally use 'Referenced be db' to create an object which stores which tables
-- to use in the automatic join queries.
--
-- > D (Referenced be db) t f = DatabaseEntity be db (TableEntity t)
--
-- This allows you to enter this data as follows
--
-- > useraddressJoin :: UserTT (Referenced Postgres AppDatabase) Ignored
-- > useraddressJoin = User Ignored Ignored (appDatabase ^. appDatabaseAddresses)
-- brittany-disable-next-binding
type family D p (t :: (* -> *) -> *) (f :: (* -> *)) where
  D PrimaryKey t f = PrimaryKey t f
  D Named t f = Named t f
  D Full t f = t f
  D (Referenced be db) t f = DatabaseEntity be db (TableEntity t)
  D (WithConstraint1 c) t f = WithConstraint1 c t f
  D Exposed1 t f = Exposed1 t f
  D a t f = a t f

-- | Pass this type to 'D' to include the full referenced object. See 'D'.
-- brittany-disable-next-binding
data Full (t :: (* -> *) -> *) (f :: (* -> *))

-- | Use this to create an object storing the table entities of the foreign keys.
-- This is needed by 'expand'. The 'D' fields are now DatabaseEntities, so you can
-- put database tables there.
--
-- > useraddressJoin :: UserTT (Referenced Postgres AppDatabase) Ignored
-- > useraddressJoin = User Ignored Ignored (appDatabase ^. appDatabaseAddresses)
-- brittany-disable-next-binding
data Referenced be (db :: (* -> *) -> * ) (t :: (* -> *) -> *) (f :: (* -> *))

-- | Internal used type to extract the meta data
-- brittany-disable-next-binding
data Exposed1 (t :: ( * -> *) -> *) (f :: (* -> *))

-- | A 'Named table f' consist of the '_id', which is of type 'PrimaryKey table f'
-- and a human readable description of type text ( 'C f Text')
-- brittany-disable-next-binding
data Named (t :: (* -> *) -> *) (f :: (* -> *)) =
    Named
      { _id :: PrimaryKey t f
      , _name :: C f Text
      } deriving (Generic)

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

instance ToJSON (PrimaryKey t Identity) => ToJSON (Named t Identity) where
  toJSON (Named i n) = object ["id" .= toJSON i, "name" .= n]

instance FromJSON (PrimaryKey t Identity) => FromJSON (Named t Identity) where
  parseJSON =
    withObject "Named t Identity" $ \v -> Named <$> v .: "id" <*> v .: "name"

instance (FromQueryText (PrimaryKey t f), FromQueryText (C f Text))
    => FromQueryText (Named t f) where
  fromQueryText p qs =
    Named <$> fromQueryText (p <> ".id") qs <*> fromQueryText p qs

instance (ToQueryText (PrimaryKey t f), ToQueryText (C f Text))
    => ToQueryText (Named t f) where
  toQueryTextPrio p (Named i name) =
    toQueryTextPrio (p <> ".id") i ++ toQueryTextPrio p name

-- | We need to know how to create a label for a record.
-- This should only be done once for each type, hence we implemented it as a typeclass.
-- For each type you want to add a label (so this will practically be almost every record type
-- in your schema), you need to create an instance of this class.
--
-- > instance ToName UserT where
-- >   name = _userName
class Table t => ToName t where
  -- | Extract the id and display name from a table 't'
  toName :: t f -> C f Text

-- | Create a named object for a type which implements 'ToName'
toNamed :: ToName t => t a -> Named t a
toNamed x = Named (primaryKey x) (toName x)

-- | Takes the full type and returns the required expanded type.
-- Mostly for internal use. As a user you should implement 'ToName'
class ToExpanded n t where
  toExpanded :: Proxy n -> t a -> D n t a

instance ToName t => ToExpanded Named t where
  toExpanded _ = toNamed

instance ToExpanded Full t where
  toExpanded _ = id

-- | This is 'Beamable' for the enhanced records. This makes sure we can zip only
-- over the foreign key fields.
--
-- Types which normally would derive 'Beamable', will now derive 'Beamable1'.
-- It only makes sense to use this for types which contains Foreign Keys, otherwise
-- there is nothing to expand.
class Beamable1 (table :: (((* -> *) -> * ) -> (* -> *) -> *) -> (* -> *) -> *)  where
    -- | Zips the foreign key fields, applying the first function to each of the
    -- foreign key fields and returning the right hand side for other, non foreign key, fields
    zipBeamFieldsM1
      :: Applicative m
      => (forall t. D' f t Ignored -> D' g t a -> m (D' h t a))
      -> table f Ignored
      -> table g a
      -> m (table h a)

    default zipBeamFieldsM1
      :: ( HasBeamFields1 table f g h a
         , Applicative m
         )
      => (forall t. D' f t Ignored -> D' g t a -> m (D' h t a))
      -> table f Ignored
      -> table g a
      -> m (table h a)

    zipBeamFieldsM1 combine f (g :: table f a) =
      to <$> gZipTables1 (Proxy :: Proxy (Rep (table Exposed1 Ignored))) combine (from f) (from g)

-- | Constraint on the type to be able to zip it.
-- brittany-disable-next-binding
type HasBeamFields1 (table ::  (((* -> *) -> * ) -> (* -> *) -> *) -> (* -> *) -> *) f g h a
  = ( GZipTables1
      f
      g
      h
      a
      (Rep (table Exposed1 Ignored))
      (Rep (table f Ignored))
      (Rep (table g a))
      (Rep (table h a))
    , Generic (table f Ignored)
    , Generic (table g a)
    , Generic (table h a)
    )

-- | Generic implementation for  zipBeamFieldsM1' of 'Beamable1'
class GZipTables1 f g h a (exposedRep :: * -> *) fRep gRep hRep where
    gZipTables1
      :: Applicative m
      => Proxy exposedRep
      -> (forall t. D' f t Ignored -> D' g t a -> m (D' h t a))
      -> fRep ()
      -> gRep ()
      -> m (hRep ())

instance ( GZipTables1 f g h a exp1 f1 g1 h1
         , GZipTables1 f g h a exp2 f2 g2 h2
         )
  => GZipTables1 f g h a (exp1 :*: exp2) (f1 :*: f2) (g1 :*: g2) (h1 :*: h2) where

  gZipTables1 _ combine ~(f1 :*: f2) ~(g1 :*: g2) =
    (:*:)
      <$> gZipTables1 (Proxy :: Proxy exp1) combine f1 g1
      <*> gZipTables1 (Proxy :: Proxy exp2) combine f2 g2


instance GZipTables1 f g h a exp fRep gRep hRep =>
    GZipTables1 f g h a (M1 x y exp) (M1 x y fRep) (M1 x y gRep) (M1 x y hRep) where
  gZipTables1 _ combine ~(M1 f) ~(M1 g) =
    M1 <$> gZipTables1 (Proxy :: Proxy exp) combine f g

-- | Here we specify what to do for foreign key fields
instance ( fa ~ D f t Ignored
         , ga ~ D g t a
         , ha ~ D h t a) =>
    GZipTables1 f g h a (K1 R (Exposed1 t Ignored)) (K1 R fa) (K1 R ga) (K1 R ha) where
  gZipTables1 _ combine ~(K1 f) ~(K1 g) =
    (\(D' h) -> K1 h) <$> combine (D' f :: D' f t Ignored) (D' g :: D' g t a)

-- | Here we specify what to do for the non-foreign key fields (just return the right hand side)
instance ( ga ~ C a b
         , ha ~ C a b) =>
    GZipTables1 f g h a (K1 R (Ignored b)) (K1 R (Ignored b)) (K1 R ga) (K1 R ha) where
  gZipTables1 _ _ _ ~(K1 g) = pure (K1 g)

-- | This is for nested types
instance ( Beamable1 tbl
         ) => GZipTables1 f g h a (K1 R (tbl Exposed1 Ignored)) (K1 R (tbl f Ignored))
                                                          (K1 R (tbl g a))
                                                          (K1 R (tbl h a))
   where
  gZipTables1 _ combine ~(K1 f) ~(K1 g) = K1 <$> zipBeamFieldsM1 combine f g

instance GZipTables1 f g h a U1 U1 U1 U1 where
  gZipTables1 _ _ _ _ = pure U1

-- | Tags a primary key field with a constraint
data WithConstraint1 (c :: ((* -> *) -> *) -> Constraint ) f a where
  WithConstraint1 ::c f => D PrimaryKey f a -> WithConstraint1 c f a

-- | Assert that all foreign key fields in the table satisfy a constraint
-- brittany-disable-next-binding
type TablesFulfillConstraint f (c :: ((* -> *) -> *) -> Constraint) (t ::(((* -> *) -> * ) -> (* -> *) -> *) -> (* -> *) -> *) =
  ( Generic (t (WithConstraint1 c) f), Generic (t PrimaryKey f), Generic (t Exposed1 Ignored)
  , GTablesFulfillConstraint c (Rep (t Exposed1 Ignored)) (Rep (t PrimaryKey f)) (Rep (t (WithConstraint1 c) f))
  )

-- | Generic check if all fields satisfy a constraint
-- brittany-disable-next-binding
class GTablesFulfillConstraint (c :: (( * -> *) -> *) -> Constraint) (exposed :: * -> *) values withconstraint where
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

instance (c t) => GTablesFulfillConstraint c (K1 R (Exposed1 t Ignored)) (K1 R (PrimaryKey t f)) (K1 R (WithConstraint1 c t f)) where
  gWithConstrainedTables _ _ (K1 x) = K1 (WithConstraint1 x)

instance GTablesFulfillConstraint c (K1 R (Ignored b)) (K1 R (f b)) (K1 R (f b)) where
  gWithConstrainedTables _ _ (K1 x) = K1 x

-- brittany-disable-next-binding
instance TablesFulfillConstraint f c t =>
    GTablesFulfillConstraint c (K1 R (t Exposed1 Ignored)) (K1 R (t PrimaryKey f)) (K1 R (t (WithConstraint1 c) f)) where
  gWithConstrainedTables _ _ (K1 x) = K1
    (to
      (gWithConstrainedTables (Proxy @c)
                              (Proxy @(Rep (t Exposed1 Ignored)))
                              (from x)
      )
    )

-- | A field has the right constraints to be used in 'expand'.
class (Table t, HasTableEquality be (PrimaryKey t), ToExpanded f t) => HasNameConstraint f be t
instance (Table t, HasTableEquality be (PrimaryKey t), ToExpanded f t) => HasNameConstraint f be t

-- | Like 'expand', but with the unexpanded table not already in the Q monad.
expand'
  :: forall table be db n s
   . ( Beamable1 table
     , Database be db
     , TablesFulfillConstraint (QExpr be s) (HasNameConstraint n be) table
     , BeamSqlBackend be
     )
  => table (Referenced be db) Ignored
  -> table PrimaryKey (QExpr be s)
  -> Q be db s (table n (QExpr be s))
expand' fkTbl tbl = zipBeamFieldsM1 (combine (Proxy :: Proxy n))
                                    fkTbl
                                    (withConstrainedTables tbl)
 where
  combine
    :: Proxy n
    -> D' (Referenced be db) t Ignored
    -> D' (WithConstraint1 (HasNameConstraint n be)) t (QExpr be s)
    -> Q be db s (D' n t (QExpr be s))
  combine p (D' ref) (D' (WithConstraint1 f)) =
    D' . toExpanded p <$> related_ ref f


-- | Adds a constraint to each primary key field of the table.
-- brittany-disable-next-binding
withConstrainedTables
  :: forall c tbl f
   . TablesFulfillConstraint f c tbl
  => tbl PrimaryKey f
  -> tbl (WithConstraint1 c) f
withConstrainedTables =
  to
    . gWithConstrainedTables (Proxy @c) (Proxy @(Rep (tbl Exposed1 Ignored)))
    . from

-- | Expand replaces @PrimaryKey@ with any @n@ implementing 'ToExpanded'.
-- Most likely you will want to use 'all_'' instead.
--
-- For this we must know which tables the foreign key are referencing to. This is
-- specified by @table (Referenced be db) Ignored@, see 'Referenced' how to do this.
--
-- === Usage
--
-- > expand useraddressJoin $ all_ (appDatabase ^. appDatabaseUsers)
--
-- Then you can proceed with adding filters, etc as you would normally do for
-- Embedded resources, see https://tathougies.github.io/beam/user-guide/models/.
--
expand
  :: ( Beamable1 table
     , Database be db
     , TablesFulfillConstraint (QExpr be s) (HasNameConstraint n be) table
     , BeamSqlBackend be
     )
  => table (Referenced be db) Ignored
  -> Q be db s (table PrimaryKey (QExpr be s))
  -> Q be db s (table n (QExpr be s))
expand fkTbl tbl = tbl >>= expand' fkTbl

-- | To be used instead of 'all_' to get results of type @t n Identity@ for some @n@
-- implementing 'ToExpanded', such as 'Named'.
--
-- For this we must know which tables the foreign key are referencing to. This is
-- specified by @table (Referenced be db) Ignored@, see 'Referenced' how to do this.
-- So it is a bit like 'allFromView_'.
--
-- === Usage
--
-- > all_' (appDatabase ^. appDatabaseUsers) useraddressJoin
--
-- Then you can proceed with adding filters, etc as you would normally do for
-- Embedded resources, see https://tathougies.github.io/beam/user-guide/models/.
--
all_'
  :: ( Beamable1 table
     , Database be db
     , TablesFulfillConstraint (QExpr be s) (HasNameConstraint n be) table
     , BeamSqlBackend be
     )
  => DatabaseEntity be db (TableEntity (table PrimaryKey))
  -> table (Referenced be db) Ignored
  -> Q be db s (table n (QExpr be s))
all_' tbl fkTbl = expand fkTbl $ all_ tbl
