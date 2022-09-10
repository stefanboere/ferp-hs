{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module ProjectM36.Beamable.Tupleable
  ( toAttributes
  , toDefineExpr
  , toDeleteExpr
  , toDeletesExpr
  , toInsertExpr
  , toPatchExpr
  , toSaveExpr
    -- * Filtering
  , references_
  , referencesAny_
  -- * Utilities
  , primaryKeyAttributeNames
  , tblFromTuplePk
  , tblFromTuple
  , tblEntityName
  , dbFilterExpr
  ) where

import           Control.Monad.Writer           ( MonadWriter(..)
                                                , Writer
                                                , execWriter
                                                )
import           Data.Foldable                  ( foldr'
                                                , toList
                                                )
import           Data.Functor.Identity          ( Identity )
import           Data.Kind                      ( Type )
import           Data.List                      ( sortOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( maybeToList )
import           Data.Monoid                    ( Last(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Data.Typeable
import           ProjectM36.Atomable            ( Atomable(..)
                                                , toAtomType
                                                )
import qualified ProjectM36.Attribute          as Attr
import           ProjectM36.Base
import           ProjectM36.DataTypes.Primitive ( atomTypeForAtom )
import           ProjectM36.Error               ( RelationalError(..) )
import           ProjectM36.Tuple               ( atomForAttributeName
                                                , mkRelationTupleFromMap
                                                )
import           ProjectM36.Tupleable.Deriving  ( ModifyText(..)
                                                , SnakeCase
                                                )

import           ProjectM36.Beamable.Class

tblEntityName :: Table t => proxy t -> RelVarName
tblEntityName p =
  modifyText (Proxy :: Proxy SnakeCase) $ Text.pack $ showsTypeRep (typeRep p)
                                                                   ""
-- TODO either implement this or delete this and implement isomorphic schemas
dbFilterExpr :: proxy (t :: (Type -> Type) -> Type) -> RestrictionPredicateExpr
dbFilterExpr _ = TruePredicate

toAttributes
  :: forall t proxy
   . (Table t, FieldsFulfillConstraint Atomable t)
  => proxy t
  -> Attributes
toAttributes _ =
  Attr.attributesFromList
    . sortOn Attr.attributeName
    $ execWriter
    $ zipBeamFieldsM f
                     (withConstraints :: t (HasConstraint Atomable))
                     (tblFieldSettings :: TableSettings t)
 where
  f
    :: forall a
     . Columnar' (HasConstraint Atomable) a
    -> Columnar' (TableField t) a
    -> Writer [Attribute] (Columnar' Proxy a)
  f (Columnar' HasConstraint) (Columnar' t) = do
    tell [Attribute (fieldAttributeName t) (toAtomType (Proxy :: Proxy a))]
    pure $ Columnar' Proxy

toTuple
  :: forall t
   . (Table t, FieldsFulfillConstraint Atomable t)
  => t Identity
  -> RelationTuple
toTuple a = mkRelationTupleFromMap . execWriter $ zipBeamFieldsM
  f
  (withConstrainedFields a :: t (WithConstraint Atomable))
  (tblFieldSettings :: TableSettings t)
 where
  f
    :: forall a
     . Columnar' (WithConstraint Atomable) a
    -> Columnar' (TableField t) a
    -> Writer (Map AttributeName Atom) (Columnar' Proxy a)
  f (Columnar' (WithConstraint v)) (Columnar' t) = do
    tell $ Map.singleton (fieldAttributeName t) (toAtom v)
    pure $ Columnar' Proxy

tblFromTuple
  :: forall t
   . (Table t, FieldsFulfillConstraint Atomable t)
  => RelationTuple
  -> Either RelationalError (t Identity)
tblFromTuple a = zipBeamFieldsM
  (readColumn a)
  (withConstraints :: t (HasConstraint Atomable))
  (tblFieldSettings :: TableSettings t)

readColumn
  :: forall a t
   . RelationTuple
  -> Columnar' (HasConstraint Atomable) a
  -> Columnar' (TableField t) a
  -> Either RelationalError (Columnar' Identity a)
readColumn a (Columnar' HasConstraint) (Columnar' t) = do
  atom <- atomForAttributeName (fieldAttributeName t) a
  val  <- atomv atom
  pure $ Columnar' val
 where
  expectedAtomType = toAtomType (Proxy :: Proxy a)
  atomv atom
    | expectedAtomType /= atomTypeForAtom atom = Left
    $ AtomTypeMismatchError expectedAtomType (atomTypeForAtom atom)
    | otherwise = Right (fromAtom atom)


tblFromTuplePk
  :: forall t
   . (Table t, FieldsFulfillConstraint Atomable (PrimaryKey t))
  => RelationTuple
  -> Either RelationalError (PrimaryKey t Identity)
tblFromTuplePk a = zipBeamFieldsM
  (readColumn a)
  (withConstraints :: PrimaryKey t (HasConstraint Atomable))
  (primaryKey (tblFieldSettings :: TableSettings t))


-- TODO foreign key, primary key constraints, other inclusion dependencys
toDefineExpr
  :: forall t
   . (Table t, FieldsFulfillConstraint Atomable t)
  => Proxy t
  -> DatabaseContextExpr
toDefineExpr tbl =
  let (attrs, typedefs) = execWriter $ zipBeamFieldsM
        f
        (withConstraints :: t (HasConstraint Atomable))
        (tblFieldSettings :: TableSettings t)
      reldef =
        Define (tblEntityName tbl) (map NakedAttributeExpr (Attr.toList attrs))
  in  if null typedefs then reldef else MultipleExpr (reldef : typedefs)
 where
  f
    :: forall a
     . Columnar' (HasConstraint Atomable) a
    -> Columnar' (TableField t) a
    -> Writer (Attributes, [DatabaseContextExpr]) (Columnar' Proxy a)
  f (Columnar' HasConstraint) (Columnar' t) = do
    tell
      ( Attr.singleton
        $ Attribute (fieldAttributeName t) (toAtomType (Proxy :: Proxy a))
      , let x = toAddTypeExpr (Proxy :: Proxy a) in [ x | x /= NoOperation ]
      )
    pure $ Columnar' Proxy


toInsertExpr
  :: forall a t
   . (Table a, Traversable t, FieldsFulfillConstraint Atomable a)
  => t (a Identity)
  -> DatabaseContextExpr
toInsertExpr vals =
  let
    tbl   = Proxy :: Proxy a
    attrs = toAttributes tbl
    rel =
      MakeStaticRelation attrs (RelationTupleSet $ toList $ fmap toTuple vals)
  in
    Insert (tblEntityName tbl) rel


toPatchExpr
  :: forall t
   . ( Table t
     , FieldsFulfillConstraintNullable Atomable t
     , FieldsFulfillConstraint Atomable (PrimaryKey t)
     )
  => PrimaryKey t Identity
  -> t Last
  -> DatabaseContextExpr
toPatchExpr pk a = Update (tblEntityName tbl)
                          updates
                          (references_ (dbFilterExpr tbl) pk)
 where
  toNullable :: Columnar' Last a -> Columnar' (Nullable Identity) a
  toNullable ~(Columnar' (Last x)) = Columnar' x

  updates = withoutPrimaryKeys tbl $ execWriter $ zipBeamFieldsM
    f
    (withNullableConstrainedFields (changeBeamRep toNullable a) :: t
        (Nullable (WithConstraint Atomable))
    )
    (tblFieldSettings :: TableSettings t)

  f
    :: forall a
     . Columnar' (Nullable (WithConstraint Atomable)) a
    -> Columnar' (TableField t) a
    -> Writer (Map AttributeName AtomExpr) (Columnar' Proxy a)
  f (Columnar' (WithConstraint v)) (Columnar' t) = do
    tell $ case toAtom v of
      ConstructedAtom "Just" _ [x] ->
        Map.singleton (fieldAttributeName t) (NakedAtomExpr x)
      _ -> mempty
    pure $ Columnar' Proxy

  tbl :: Proxy t
  tbl = Proxy

withoutPrimaryKeys
  :: Table t => proxy t -> Map AttributeName a -> Map AttributeName a
withoutPrimaryKeys p = (`Map.withoutKeys` primaryKeyAttributeNames p)

primaryKeyAttributeNames
  :: forall t proxy . Table t => proxy t -> Set AttributeName
primaryKeyAttributeNames _ = execWriter
  $ zipBeamFieldsM g tblSkeleton (primaryKey tblFieldSettings)
 where
  g
    :: forall a
     . Columnar' Proxy a
    -> Columnar' (TableField t) a
    -> Writer (Set AttributeName) (Columnar' Proxy a)
  g _ (Columnar' tbl') = do
    tell $ Set.singleton $ fieldAttributeName tbl'
    pure $ Columnar' Proxy

toSaveExpr
  :: forall t
   . ( Table t
     , FieldsFulfillConstraint Atomable t
     , FieldsFulfillConstraint Atomable (PrimaryKey t)
     )
  => PrimaryKey t Identity -- ^ Overwites the primary key of the value
  -> t Identity
  -> DatabaseContextExpr
toSaveExpr pk a = Update (tblEntityName tbl)
                         updates
                         (references_ (dbFilterExpr tbl) pk)
 where
  updates = withoutPrimaryKeys tbl $ execWriter $ zipBeamFieldsM
    f
    (withConstrainedFields a :: t (WithConstraint Atomable))
    (tblFieldSettings :: TableSettings t)

  f
    :: forall a
     . Columnar' (WithConstraint Atomable) a
    -> Columnar' (TableField t) a
    -> Writer (Map AttributeName AtomExpr) (Columnar' Proxy a)
  f (Columnar' (WithConstraint v)) (Columnar' t) = do
    tell $ Map.singleton (fieldAttributeName t) (NakedAtomExpr $ toAtom v)
    pure $ Columnar' Proxy

  tbl :: Proxy t
  tbl = Proxy

toDeleteExpr
  :: forall t
   . (Table t, FieldsFulfillConstraint Atomable (PrimaryKey t))
  => PrimaryKey t Identity
  -> DatabaseContextExpr
toDeleteExpr pk = Delete (tblEntityName tbl)
                         (references_ (dbFilterExpr tbl) pk)
 where
  tbl :: Proxy t
  tbl = Proxy

toDeletesExpr
  :: forall t f
   . ( Table t
     , FieldsFulfillConstraint Atomable (PrimaryKey t)
     , Foldable f
     , Functor f
     )
  => f (PrimaryKey t Identity)
  -> DatabaseContextExpr
toDeletesExpr pks = Delete (tblEntityName tbl)
  $ AndPredicate (dbFilterExpr tbl) (referencesAny_ pks)
 where
  tbl :: Proxy t
  tbl = Proxy

referencesAny_
  :: ( Table t
     , FieldsFulfillConstraint Atomable (PrimaryKey t)
     , Foldable f
     , Functor f
     )
  => f (PrimaryKey t Identity)
  -> RestrictionPredicateExpr
referencesAny_ pks = foldr' OrPredicate
                            (NotPredicate TruePredicate)
                            (fmap (references_ TruePredicate) pks)

references_
  :: (Table t, FieldsFulfillConstraint Atomable (PrimaryKey t))
  => RestrictionPredicateExpr
  -> PrimaryKey t Identity
  -> RestrictionPredicateExpr
references_ orig pk = foldr' AndPredicate orig $ execWriter $ zipBeamFieldsM
  f
  (primaryKey tblFieldSettings)
  (val_ pk)
 where
  f
    :: Columnar' (TableField t) a
    -> Columnar' QGenExpr a
    -> Writer [RestrictionPredicateExprBase ()] (Columnar' Proxy a)
  f (Columnar' tbl) (Columnar' (QGenExpr x)) = do
    tell
      ( maybeToList
      . fmap
          ( AttributeEqualityPredicate (fieldAttributeName tbl)
          . NakedAtomExpr
          )
      $ x
      )
    pure $ Columnar' Proxy

