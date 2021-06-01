{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module: Database.Beam.Operator
Description: Beam implementation of Servant.Crud.Operator

Converts a 'table Filter' to Beam filters, so that this can be used in database queries.

-}
module Database.Beam.Operator
  ( matching_
  , Operator(..)
  , FieldsFulfillConstraintFilter
  )
where

import           Prelude

import           Data.Kind                      ( Constraint )
import           Data.Functor.Identity
import           Data.Proxy                     ( Proxy(..) )
import           Data.String                    ( IsString )
import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Schema.Tables
import           GHC.Generics                   ( Rep
                                                , to
                                                , K1(..)
                                                , R
                                                )
import           Servant.Crud.QueryOperator


-- | This specifies how an operator @c@ with value type @a@ can be converted to
-- a filter in the Beam EDSL. @be@ is the backend, i.e. 'Postgres'.
--
-- Instances are provided here for all the standard operators from "Servant.Crud.QueryOperator",
-- but if you have custom operators, this is where you need to write an instance for.
-- In this case write an instance for 'OpEntry operator valuetype'.
class Operator c be a where
  passes :: BeamSqlBackend be
    => c a  -- ^ The condition, in the spirit of \" > 5 \"
    -> QGenExpr ctxt be s a  -- ^ The value to match, e.g. 6, but then as a Beam Expression
    -> Maybe (QGenExpr ctxt be s Bool)
    -- ^ The result of the match, e.g. True (since 6 > 5), but then as a Beam Expression.
    -- Is 'Nothing' if no filter should be applied.

instance Operator (OpDict '[]) be a where
  passes Nil _ = Nothing

instance (Operator (OpEntry s k) be a, Operator (OpDict xs) be a)
  => Operator (OpDict ( '(s, k) ': xs)) be a where
  passes (x :> xs) y = passes x y <&&> passes xs y
   where
    Nothing <&&> b       = b
    a       <&&> Nothing = a
    Just a  <&&> Just b  = Just (a &&. b)


instance Operator (OpDict (DefaultFilters a)) be a => Operator Filter be a where
  passes (Filter x) = passes x

-- | Wraps a type of 'Filter a' with a constraint
data WithConstraintFilter (c :: * -> Constraint) x where
  WithConstraintFilter ::c x => Filter x -> WithConstraintFilter c x

data HasConstraintFilter (c :: * -> Constraint) x where
  HasConstraintFilter ::c x => HasConstraintFilter c x

-- | All fields in a table @t@ satisfy the constraint @c@. Is used to specify the
-- constraints for 'matching_'.
--
-- Mostly for internal use.
type FieldsFulfillConstraintFilter (c :: * -> Constraint) t
  = ( Generic (t (HasConstraintFilter c))
    , Generic (t Filter)
    , Generic (t Exposed)
    , GFieldsFulfillConstraint
        c
        (Rep (t Exposed))
        (Rep (t (HasConstraintFilter c)))
    )

instance FieldsFulfillConstraintFilter c t =>
    GFieldsFulfillConstraint c (K1 R (t Exposed)) (K1 R (t (HasConstraintFilter c))) where
  gWithConstrainedFields _ _ =
    K1 (to (gWithConstrainedFields (Proxy @c) (Proxy @(Rep (t Exposed)))))

instance (c x) => GFieldsFulfillConstraint c (K1 R (Exposed x)) (K1 R (HasConstraintFilter c x)) where
  gWithConstrainedFields _ _ = K1 HasConstraintFilter

withConstrainedFieldsFilter
  :: forall c tbl
   . (FieldsFulfillConstraintFilter c tbl, Beamable tbl)
  => tbl Filter
  -> tbl (WithConstraintFilter c)
withConstrainedFieldsFilter = runIdentity
  . zipBeamFieldsM f (withConstraintsFilter @c @tbl)
 where
  f
    :: forall a
     . Columnar' (HasConstraintFilter c) a
    -> Columnar' Filter a
    -> Identity (Columnar' (WithConstraintFilter c) a)
  f (Columnar' HasConstraintFilter) (Columnar' a) =
    Identity $ Columnar' $ WithConstraintFilter a

withConstraintsFilter
  :: forall c tbl
   . (FieldsFulfillConstraintFilter c tbl)
  => tbl (HasConstraintFilter c)
withConstraintsFilter =
  to $ gWithConstrainedFields (Proxy @c) (Proxy @(Rep (tbl Exposed)))


-- | Return only those rows from a collection which match the filter.
--
-- The constrains are that all operators in @table Filter@ have a proper 'Operator' instance.
matching_
  :: ( BeamSqlBackend be
     , Beamable table
     , FieldsFulfillConstraintFilter (Operator Filter be) table
     )
  => table Filter -- ^ The filters to match
  -> Q be db s (table (QExpr be s)) -- ^ The original collection
  -> Q be db s (table (QExpr be s)) -- ^ The rows from the original collection matching all the filters
matching_ = matching_' Proxy

-- | Like 'matching_', but with a proxy
matching_'
  :: ( BeamSqlBackend be
     , Beamable table
     , FieldsFulfillConstraintFilter (Operator Filter be) table
     )
  => Proxy be
  -> table Filter
  -> Q be db s (table (QExpr be s))
  -> Q be db s (table (QExpr be s))
matching_' p tblFilter tbl = do
  allTbl <- tbl
  zipBeamFieldsM
    (\(Columnar' (WithConstraintFilter cond)) (Columnar' val) ->
      maybe (pure ()) guard_ (cond `passes` val) >> pure (Columnar' val)
    )
    (addConstraint p tblFilter)
    allTbl
 where
  addConstraint
    :: ( Beamable table
       , FieldsFulfillConstraintFilter (Operator Filter be) table
       )
    => Proxy be
    -> table Filter
    -> table (WithConstraintFilter (Operator Filter be))
  addConstraint _ = withConstrainedFieldsFilter

-- * Default instances
instance (BeamSqlBackendCanSerialize be a, HasSqlEqualityCheck be a)
    => Operator (OpEntry "" 'List) be a where
  passes (E [] ) _ = Nothing
  passes (E [x]) y = Just $ y ==. val_ x
  passes (E xs ) y = Just $ y `in_` map val_ xs

instance (BeamSqlBackendCanSerialize be a, HasSqlEqualityCheck be a)
    => Operator (OpEntry "!" 'List) be a where
  passes (E [] ) _ = Nothing
  passes (E [x]) y = Just $ y /=. val_ x
  passes (E xs ) y = Just $ not_ $ y `in_` map val_ xs


instance Operator (OpEntry "null" 'Flag) be (Maybe a) where
  passes (E NotPresent) _ = Nothing
  passes (E Present   ) y = Just $ isNothing_ y

instance Operator (OpEntry "!null" 'Flag) be (Maybe a) where
  passes (E NotPresent) _ = Nothing
  passes (E Present   ) y = Just $ isJust_ y

-- | Helper function for passes function for Ord instances
passesOrd
  :: (BeamSqlBackendCanSerialize be a, BeamSqlBackend be)
  => (  QGenExpr ctxt be s a
     -> QGenExpr ctxt be s a
     -> QGenExpr ctxt be s Bool
     )
  -> OpEntry sym 'Normal a
  -> QGenExpr ctxt be s a
  -> Maybe (QGenExpr ctxt be s Bool)
passesOrd _    (E (MaybeLast Nothing )) _ = Nothing
passesOrd comp (E (MaybeLast (Just x))) y = Just $ y `comp` val_ x

instance (BeamSqlBackendCanSerialize be a)
    => Operator (OpEntry "gt" 'Normal) be a where
  passes = passesOrd (>.)

instance (BeamSqlBackendCanSerialize be a)
    => Operator (OpEntry "lt" 'Normal) be a where
  passes = passesOrd (<.)

instance (BeamSqlBackendCanSerialize be a)
    => Operator (OpEntry "ge" 'Normal) be a where
  passes = passesOrd (>=.)

instance (BeamSqlBackendCanSerialize be a)
    => Operator (OpEntry "le" 'Normal) be a where
  passes = passesOrd (>=.)

-- | Helper for positive string tests
passesStr
  :: ( BeamSqlBackendIsString be a
     , BeamSqlBackend be
     , BeamSqlBackendCanSerialize be a
     )
  => (a -> a)
  -> OpEntry sym 'Normal a
  -> QGenExpr ctxt be s a
  -> Maybe (QGenExpr ctxt be s Bool)
passesStr _ (E (MaybeLast Nothing)) _ = Nothing
passesStr f (E (MaybeLast (Just x))) y =
  Just $ lower_ y `like_` lower_ (val_ (f x))

-- | Helper for negative string tests
passesStrN
  :: ( BeamSqlBackendIsString be a
     , BeamSqlBackend be
     , BeamSqlBackendCanSerialize be a
     )
  => (a -> a)
  -> OpEntry sym 'Normal a
  -> QGenExpr ctxt be s a
  -> Maybe (QGenExpr ctxt be s Bool)
passesStrN _ (E (MaybeLast Nothing)) _ = Nothing
passesStrN f (E (MaybeLast (Just x))) y =
  Just $ not_ $ lower_ y `like_` lower_ (val_ (f x))

instance ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "start" 'Normal) be a where
  passes = passesStr (<> "%")

instance ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "!start" 'Normal) be a where
  passes = passesStrN (<> "%")

instance ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "end" 'Normal) be a where
  passes = passesStr ("%" <>)

instance ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "!end" 'Normal) be a where
  passes = passesStrN ("%" <>)

instance ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "contains" 'Normal) be a where
  passes = passesStr (\x -> "%" <> x <> "%")

instance ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "!contains" 'Normal) be a where
  passes = passesStrN (\x -> "%" <> x <> "%")

instance ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a)
    => Operator (OpEntry "match" 'Normal) be a where
  passes = passesStr id

instance ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a)
    => Operator (OpEntry "!match" 'Normal) be a where
  passes = passesStrN id



-- Variants for Maybe
-- | Helper for positive string tests with nullable fields
passesStrMaybe
  :: ( BeamSqlBackendIsString be a
     , BeamSqlBackend be
     , BeamSqlBackendCanSerialize be a
     )
  => (a -> a)
  -> OpEntry sym 'Normal (Maybe a)
  -> QGenExpr ctxt be s (Maybe a)
  -> Maybe (QGenExpr ctxt be s Bool)
passesStrMaybe _ (E (MaybeLast Nothing       )) _ = Nothing
passesStrMaybe _ (E (MaybeLast (Just Nothing))) _ = Nothing
passesStrMaybe f (E (MaybeLast (Just (Just x)))) y =
  Just $ maybe_ (val_ False) (\z -> lower_ z `like_` lower_ (val_ (f x))) y

-- | Helper for positive string tests with nullable fields
passesStrMaybeN
  :: ( BeamSqlBackendIsString be a
     , BeamSqlBackend be
     , BeamSqlBackendCanSerialize be a
     )
  => (a -> a)
  -> OpEntry sym 'Normal (Maybe a)
  -> QGenExpr ctxt be s (Maybe a)
  -> Maybe (QGenExpr ctxt be s Bool)
passesStrMaybeN _ (E (MaybeLast Nothing        )) _ = Nothing
passesStrMaybeN _ (E (MaybeLast (Just Nothing ))) _ = Nothing
passesStrMaybeN f (E (MaybeLast (Just (Just x)))) y = Just
  $ maybe_ (val_ True) (\z -> not_ $ lower_ z `like_` lower_ (val_ (f x))) y

instance {-# OVERLAPPING  #-} ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "start" 'Normal) be (Maybe a) where
  passes = passesStrMaybe (<> "%")

instance {-# OVERLAPPING  #-} ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "!start" 'Normal) be (Maybe a) where
  passes = passesStrMaybeN (<> "%")

instance {-# OVERLAPPING  #-} ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "end" 'Normal) be (Maybe a) where
  passes = passesStrMaybe ("%" <>)

instance {-# OVERLAPPING  #-} ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "!end" 'Normal) be (Maybe a) where
  passes = passesStrMaybeN ("%" <>)

instance {-# OVERLAPPING  #-} ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "contains" 'Normal) be (Maybe a) where
  passes = passesStrMaybe (\x -> "%" <> x <> "%")

instance {-# OVERLAPPING  #-} ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a
         , Semigroup a
         , IsString a
         )
    => Operator (OpEntry "!contains" 'Normal) be (Maybe a) where
  passes = passesStrMaybeN (\x -> "%" <> x <> "%")

instance {-# OVERLAPPING  #-} ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a)
    => Operator (OpEntry "match" 'Normal) be (Maybe a) where
  passes = passesStrMaybe id

instance {-# OVERLAPPING  #-} ( BeamSqlBackendIsString be a
         , BeamSqlBackendCanSerialize be a)
    => Operator (OpEntry "!match" 'Normal) be (Maybe a) where
  passes = passesStrMaybeN id
