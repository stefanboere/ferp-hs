{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module: ProjectM36.Beamable.Class
Description: Beam like definitions for ProjectM36

This module is based on the wonderfull @beam-core@ package by Travis Athougies and the Beam Authors.
Substantial portions of this module are copied from this package.
|-}
module ProjectM36.Beamable.Class
  ( module ProjectM36.Beamable.Class
  ) where

import           Data.Bifunctor                 ( first )
import           Data.Char                      ( isUpper
                                                , toLower
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Functor.Identity
import qualified Data.List.NonEmpty            as NE
import           Data.Proxy
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits
import           GHC.Types
import           ProjectM36.Atomable
import           ProjectM36.Base                ( Atom )

newtype Columnar' f a = Columnar' (Columnar f a)

type family Columnar (f :: Type -> Type) x where
  Columnar Exposed x = Exposed x
  Columnar Identity x = x
  Columnar (Nullable f) x = Columnar f (Maybe x)
  Columnar f x = f x

type C f a = Columnar f a

data Exposed x

-- brittany-disable-next-binding
data Nullable (c :: Type -> Type) x

class (Typeable table, Beamable table, Beamable (PrimaryKey table)) => Table (table :: (Type -> Type) -> Type) where
  data PrimaryKey table (column :: Type -> Type) :: Type
  primaryKey :: table column -> PrimaryKey table column

  tblFieldSettings :: TableSettings table
  default tblFieldSettings
    :: ( Generic (TableSettings table)
       , GDefaultTableFieldSettings (Rep (TableSettings table) ())
       )
    => TableSettings table
  tblFieldSettings = defTblFieldSettings

class Beamable table where
  zipBeamFieldsM
    :: Applicative m
    => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
    -> table f
    -> table g
    -> m (table h)
  default zipBeamFieldsM
    :: ( HasBeamFields table f g h
       , Applicative m
       )
    => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
    -> table f
    -> table g
    -> m (table h)
  zipBeamFieldsM combine (f :: table f) g =
    to' <$> gZipTables (Proxy :: Proxy (Rep (table Exposed))) combine (from' f) (from' g)

  tblSkeleton :: table Proxy
  default tblSkeleton
    :: ( Generic (table Proxy)
       , GTableSkeleton (Rep (table Proxy))
       )
    => table Proxy
  tblSkeleton = withProxy $ \proxy -> to' (gTblSkeleton proxy)
    where withProxy :: (Proxy (Rep (table Proxy)) -> table Proxy) -> table Proxy
          withProxy f = f Proxy

changeBeamRep
  :: Beamable table
  => (forall a . Columnar' f a -> Columnar' g a)
  -> table f
  -> table g
changeBeamRep f tbl =
  runIdentity (zipBeamFieldsM (\x _ -> return (f x)) tbl tbl)

from' :: Generic x => x -> Rep x ()
from' = from

to' :: Generic x => Rep x () -> x
to' = to

type HasBeamFields table f g h
  = ( GZipTables
        f
        g
        h
        (Rep (table Exposed))
        (Rep (table f))
        (Rep (table g))
        (Rep (table h))
    , Generic (table f)
    , Generic (table g)
    , Generic (table h)
    )

-- brittany-disable-next-binding
data TableField (table :: (Type -> Type) -> Type) ty
  = TableField
  { _fieldPath :: NE.NonEmpty Text
    -- ^ The path that led to this field. Each element is the haskell
    -- name of the record field in which this table is stored.
  , _fieldName :: Text  -- ^ The field name
  } deriving stock (Show, Eq)

fieldAttributeName :: TableField t a -> Text
fieldAttributeName = Text.intercalate "." . NE.toList . _fieldPath

type TableSettings table = table (TableField table)

defTblFieldSettings
  :: ( Generic (TableSettings table)
     , GDefaultTableFieldSettings (Rep (TableSettings table) ())
     )
  => TableSettings table
defTblFieldSettings = withProxy $ \proxy -> to' (gDefTblFieldSettings proxy)
 where
  withProxy
    :: (Proxy (Rep (TableSettings table) ()) -> TableSettings table)
    -> TableSettings table
  withProxy f = f Proxy

class GDefaultTableFieldSettings x where
    gDefTblFieldSettings :: Proxy x -> x

instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (D1 f p x) where
  gDefTblFieldSettings (_ :: Proxy (D1 f p x)) =
    M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))

instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (C1 f p x) where
  gDefTblFieldSettings (_ :: Proxy (C1 f p x)) =
    M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))

instance (GDefaultTableFieldSettings (a p), GDefaultTableFieldSettings (b p))
  => GDefaultTableFieldSettings ((a :*: b) p) where
  gDefTblFieldSettings (_ :: Proxy ((a :*: b) p)) =
    gDefTblFieldSettings (Proxy :: Proxy (a p))
      :*: gDefTblFieldSettings (Proxy :: Proxy (b p))

instance Selector f
  => GDefaultTableFieldSettings (S1 f (K1 R (TableField table field)) p) where
  gDefTblFieldSettings (_ :: Proxy (S1 f (K1 R (TableField table field)) p)) =
    M1 (K1 s)
   where
    s    = TableField (pure rawSelName) name
    name = unCamelCaseSel rawSelName
    rawSelName =
      Text.pack (selName (undefined :: S1 f (K1 R (TableField table field)) ()))

instance ( TypeError ('Text "All Beamable types must be record types, so appropriate names can be given to columns"))
  => GDefaultTableFieldSettings (K1 r f p) where
  gDefTblFieldSettings _ = error "impossible"

-- | Type-level representation of the naming strategy to use for defaulting
--   Needed because primary keys should be named after the default naming of
--   their corresponding table, not the names of the record selectors in the
--   primary key (if any).
data SubTableStrategy
  = PrimaryKeyStrategy
  | BeamableStrategy
  | RecursiveKeyStrategy

type family ChooseSubTableStrategy (tbl :: (Type -> Type) -> Type) (sub :: (Type -> Type) -> Type) :: SubTableStrategy where
  ChooseSubTableStrategy tbl (PrimaryKey tbl) = 'RecursiveKeyStrategy
  ChooseSubTableStrategy tbl (PrimaryKey rel) = 'PrimaryKeyStrategy
  ChooseSubTableStrategy tbl sub = 'BeamableStrategy

type family CheckNullable (f :: Type -> Type) :: Constraint where
  CheckNullable (Nullable f) = ()
  CheckNullable f = TypeError ('Text "Recursive references without Nullable constraint form an infinite loop." ':$$:
                               'Text "Hint: Only embed nullable 'PrimaryKey tbl' within the definition of 'tbl'." ':$$:
                               'Text "      For example, replace 'PrimaryKey tbl f' with 'PrimaryKey tbl (Nullable f)'")


class SubTableStrategyImpl (strategy :: SubTableStrategy) (f :: Type -> Type) sub where
  namedSubTable :: Proxy strategy -> sub f

-- The defaulting with @TableField rel@ is necessary to avoid infinite loops
instance ( Table rel, Generic (rel (TableField rel))
         , TagReducesTo f (TableField tbl)
         , GDefaultTableFieldSettings (Rep (rel (TableField rel)) ()) ) =>
  SubTableStrategyImpl 'PrimaryKeyStrategy f (PrimaryKey rel) where
  namedSubTable _ = primaryKey tbl
   where
    tbl =
      changeBeamRep
          (\(Columnar' (TableField path nm) :: Columnar' (TableField rel) a) ->
            let c =
                  Columnar' (TableField path nm) :: Columnar' (TableField tbl) a
            in  runIdentity (reduceTag (\_ -> pure c) undefined)
          )
        $ to'
        $ gDefTblFieldSettings (Proxy :: Proxy (Rep (rel (TableField rel)) ()))
instance ( Generic (sub f)
         , GDefaultTableFieldSettings (Rep (sub f) ()) ) =>
         SubTableStrategyImpl 'BeamableStrategy f sub where
  namedSubTable _ =
    to' $ gDefTblFieldSettings (Proxy :: Proxy (Rep (sub f) ()))
instance ( CheckNullable f, SubTableStrategyImpl 'PrimaryKeyStrategy f (PrimaryKey rel) ) =>
         SubTableStrategyImpl 'RecursiveKeyStrategy f (PrimaryKey rel) where
  namedSubTable _ = namedSubTable (Proxy :: Proxy 'PrimaryKeyStrategy)

instance {-# OVERLAPPING #-}
         ( Selector f'
         , ChooseSubTableStrategy tbl sub ~ strategy
         , SubTableStrategyImpl strategy f sub
         , TagReducesTo f (TableField tbl)
         , Beamable sub ) =>
         GDefaultTableFieldSettings (S1 f' (K1 R (sub f)) p) where
  gDefTblFieldSettings _ = M1 . K1 $ settings'
   where
    tbl :: sub f
    tbl         = namedSubTable (Proxy :: Proxy strategy)

    origSelName = Text.pack (selName (undefined :: S1 f' (K1 R (sub f)) p))
    relName     = unCamelCaseSel origSelName

    settings' :: sub f
    settings' = changeBeamRep
      (reduceTag `over` \(Columnar' (TableField path nm)) -> Columnar'
        (TableField (pure origSelName <> path) (relName <> "__" <> nm))
      )
      tbl

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over = coerce
{-# INLINE over #-}

type family ReplaceBaseTag tag f where
  ReplaceBaseTag tag (Nullable f) = Nullable (ReplaceBaseTag tag f)
  ReplaceBaseTag tag x = tag

-- | Class to automatically unwrap nested Nullables
class TagReducesTo f f' | f -> f' where
  reduceTag :: Functor m =>
               (Columnar' f' a' -> m (Columnar' f' a'))
            -> Columnar' f a -> m (Columnar' f a)
instance TagReducesTo (TableField tbl) (TableField tbl) where
  reduceTag f ~(Columnar' (TableField path nm)) =
    (\(Columnar' (TableField path' nm')) -> Columnar' (TableField path' nm'))
      <$> f (Columnar' (TableField path nm))
instance TagReducesTo f f' => TagReducesTo (Nullable f) f' where
  reduceTag fn ~(Columnar' x :: Columnar' (Nullable f) a) =
    (\(Columnar' x' :: Columnar' f (Maybe a')) -> Columnar' x')
      <$> reduceTag fn (Columnar' x :: Columnar' f (Maybe a))


unCamelCase :: Text -> [Text]
unCamelCase "" = []
unCamelCase s
  | (comp, next) <- Text.break isUpper s
  , not (Text.null comp)
  = let next' =
          maybe mempty (uncurry Text.cons . first toLower) (Text.uncons next)
    in  Text.toLower comp : unCamelCase next'
  | otherwise
  = let (comp, next) = Text.span isUpper s
        next' =
          maybe mempty (uncurry Text.cons . first toLower) (Text.uncons next)
    in  Text.toLower comp : unCamelCase next'

unCamelCaseSel :: Text -> Text
unCamelCaseSel original =
  let symbolLeft = Text.dropWhile (== '_') original
  in  if Text.null symbolLeft
        then original
        else if Text.any (== '_') symbolLeft
          then symbolLeft
          else case unCamelCase symbolLeft of
            []     -> symbolLeft
            [ xs]  -> xs
            _ : xs -> Text.intercalate "_" xs

class GZipTables f g h (exposedRep :: Type -> Type) fRep gRep hRep where
  gZipTables
    :: Applicative m
    => Proxy exposedRep
    -> (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
    -> fRep ()
    -> gRep ()
    -> m (hRep ())

instance (GZipTables f g h exp1 f1 g1 h1, GZipTables f g h exp2 f2 g2 h2)
  => GZipTables f g h (exp1 :*: exp2) (f1 :*: f2) (g1 :*: g2) (h1 :*: h2) where
  gZipTables _ combine ~(f1 :*: f2) ~(g1 :*: g2) =
    (:*:)
      <$> gZipTables (Proxy :: Proxy exp1) combine f1 g1
      <*> gZipTables (Proxy :: Proxy exp2) combine f2 g2

instance GZipTables f g h exp fRep gRep hRep
  => GZipTables f g h (M1 x y exp) (M1 x y fRep) (M1 x y gRep) (M1 x y hRep) where
  gZipTables _ combine ~(M1 f) ~(M1 g) =
    M1 <$> gZipTables (Proxy :: Proxy exp) combine f g

instance (fa ~ Columnar f a, ga ~ Columnar g a, ha ~ Columnar h a)
  => GZipTables f g h (K1 R (Exposed a)) (K1 R fa) (K1 R ga) (K1 R ha) where
  gZipTables _ combine ~(K1 f) ~(K1 g) = (\(Columnar' h) -> K1 h)
    <$> combine (Columnar' f :: Columnar' f a) (Columnar' g :: Columnar' g a)

instance (Beamable tbl)
  => GZipTables f g h (K1 R (tbl Exposed)) (K1 R (tbl f)) (K1 R (tbl g)) (K1 R (tbl h)) where
  gZipTables _ combine ~(K1 f) ~(K1 g) = K1 <$> zipBeamFieldsM combine f g

instance GZipTables f g h U1 U1 U1 U1 where
  gZipTables _ _ _ _ = pure U1

instance (Beamable tbl)
  => GZipTables f g h (K1 R (tbl (Nullable Exposed))) (K1 R (tbl (Nullable f))) (K1 R (tbl (Nullable g))) (K1 R (tbl (Nullable h))) where
  gZipTables _ combine ~(K1 f) ~(K1 g) =
    K1 <$> zipBeamFieldsM (adapt combine) f g
   where
    adapt
      :: Applicative m
      => (forall a . Columnar' f a -> Columnar' g a -> m (Columnar' h a))
      -> (  forall a
          . Columnar' (Nullable f) a
         -> Columnar' (Nullable g) a
         -> m (Columnar' (Nullable h) a)
         )
    adapt func x y = toNullable <$> func (fromNullable x) (fromNullable y)

    fromNullable :: Columnar' (Nullable w) a -> Columnar' w (Maybe a)
    fromNullable ~(Columnar' x) = Columnar' x

    toNullable :: Columnar' w (Maybe a) -> Columnar' (Nullable w) a
    toNullable ~(Columnar' x) = Columnar' x

class GTableSkeleton x where
  gTblSkeleton :: Proxy x -> x ()

instance GTableSkeleton p => GTableSkeleton (M1 t f p) where
  gTblSkeleton (_ :: Proxy (M1 t f p)) = M1 (gTblSkeleton (Proxy :: Proxy p))

instance GTableSkeleton U1 where
  gTblSkeleton _ = U1

instance (GTableSkeleton a, GTableSkeleton b) => GTableSkeleton (a :*: b) where
  gTblSkeleton _ =
    gTblSkeleton (Proxy :: Proxy a) :*: gTblSkeleton (Proxy :: Proxy b)

instance GTableSkeleton (K1 R (Proxy field)) where
  gTblSkeleton _ = K1 Proxy

instance Beamable tbl => GTableSkeleton (K1 R (tbl Proxy)) where
  gTblSkeleton _ = K1 (tblSkeleton :: tbl Proxy)

instance Beamable tbl => GTableSkeleton (K1 R (tbl (Nullable Proxy))) where
  gTblSkeleton _ = K1 . runIdentity $ zipBeamFieldsM
    transform
    (tblSkeleton :: tbl Proxy)
    (tblSkeleton :: tbl Proxy)
   where
    transform
      :: Columnar' Proxy a
      -> Columnar' Proxy a
      -> Identity (Columnar' (Nullable Proxy) a)
    transform _ _ = Identity (Columnar' Proxy)

type FieldsFulfillConstraint (c :: Type -> Constraint) t
  = ( Generic (t (HasConstraint c))
    , Generic (t Identity)
    , Generic (t Exposed)
    , GFieldsFulfillConstraint c (Rep (t Exposed)) (Rep (t (HasConstraint c)))
    )

type FieldsFulfillConstraintNullable (c :: Type -> Constraint) t
  = ( Generic (t (Nullable (HasConstraint c)))
    , Generic (t (Nullable Identity))
    , Generic (t (Nullable Exposed))
    , GFieldsFulfillConstraint
        c
        (Rep (t (Nullable Exposed)))
        (Rep (t (Nullable (HasConstraint c))))
    )

data WithConstraint (c :: Type -> Constraint) x where
  WithConstraint ::c x => x -> WithConstraint c x

data HasConstraint (c :: Type -> Constraint) x where
  HasConstraint ::c x => HasConstraint c x

class GFieldsFulfillConstraint (c :: Type -> Constraint) (exposed :: Type -> Type) withconstraint where
  gWithConstrainedFields :: Proxy c -> Proxy exposed -> withconstraint ()

instance GFieldsFulfillConstraint c exposed withconstraint
  => GFieldsFulfillConstraint c (M1 s m exposed) (M1 s m withconstraint) where
  gWithConstrainedFields c _ =
    M1 (gWithConstrainedFields c (Proxy :: Proxy exposed))

instance GFieldsFulfillConstraint c U1 U1 where
  gWithConstrainedFields _ _ = U1

instance (GFieldsFulfillConstraint c aExp aC, GFieldsFulfillConstraint c bExp bC)
  => GFieldsFulfillConstraint c (aExp :*: bExp) (aC :*: bC) where
  gWithConstrainedFields be _ =
    gWithConstrainedFields be (Proxy :: Proxy aExp)
      :*: gWithConstrainedFields be (Proxy :: Proxy bExp)

instance (c x) => GFieldsFulfillConstraint c (K1 R (Exposed x)) (K1 R (HasConstraint c x)) where
  gWithConstrainedFields _ _ = K1 HasConstraint

instance FieldsFulfillConstraint c t
  => GFieldsFulfillConstraint c (K1 R (t Exposed)) (K1 R (t (HasConstraint c))) where
  gWithConstrainedFields _ _ = K1
    (to
      (gWithConstrainedFields (Proxy :: Proxy c)
                              (Proxy :: Proxy (Rep (t Exposed)))
      )
    )

instance FieldsFulfillConstraintNullable c t
  => GFieldsFulfillConstraint c (K1 R (t (Nullable Exposed))) (K1 R (t (Nullable (HasConstraint c)))) where
  gWithConstrainedFields _ _ = K1
    (to
      (gWithConstrainedFields (Proxy :: Proxy c)
                              (Proxy :: Proxy (Rep (t (Nullable Exposed))))
      )
    )

withConstrainedFields
  :: forall c tbl
   . (FieldsFulfillConstraint c tbl, Beamable tbl)
  => tbl Identity
  -> tbl (WithConstraint c)
withConstrainedFields = runIdentity
  . zipBeamFieldsM f (withConstraints :: tbl (HasConstraint c))
 where
  f
    :: forall a
     . Columnar' (HasConstraint c) a
    -> Columnar' Identity a
    -> Identity (Columnar' (WithConstraint c) a)
  f (Columnar' HasConstraint) (Columnar' a) =
    Identity $ Columnar' $ WithConstraint a

withConstraints
  :: forall c tbl . (FieldsFulfillConstraint c tbl) => tbl (HasConstraint c)
withConstraints = to $ gWithConstrainedFields
  (Proxy :: Proxy c)
  (Proxy :: Proxy (Rep (tbl Exposed)))

withNullableConstrainedFields
  :: forall c tbl
   . (FieldsFulfillConstraintNullable c tbl, Beamable tbl)
  => tbl (Nullable Identity)
  -> tbl (Nullable (WithConstraint c))
withNullableConstrainedFields = runIdentity . zipBeamFieldsM
  f
  (withNullableConstraints :: tbl (Nullable (HasConstraint c)))
 where
  f
    :: forall a
     . Columnar' (Nullable (HasConstraint c)) a
    -> Columnar' (Nullable Identity) a
    -> Identity (Columnar' (Nullable (WithConstraint c)) a)
  f (Columnar' HasConstraint) (Columnar' a) =
    Identity $ Columnar' $ WithConstraint a

withNullableConstraints
  :: forall c tbl
   . (FieldsFulfillConstraintNullable c tbl)
  => tbl (Nullable (HasConstraint c))
withNullableConstraints = to $ gWithConstrainedFields
  (Proxy :: Proxy c)
  (Proxy :: Proxy (Rep (tbl (Nullable Exposed))))

type family HaskellLiteralForQExpr x = a
type instance HaskellLiteralForQExpr (QGenExpr a) = a
type instance HaskellLiteralForQExpr (table QGenExpr) = table Identity
type instance HaskellLiteralForQExpr (table (Nullable f))
  = HaskellLiteralForQExprAddNullable (HaskellLiteralForQExpr (table f))

type family HaskellLiteralForQExprAddNullable x = a
type instance HaskellLiteralForQExprAddNullable (tbl f) = tbl (Nullable f)

newtype QGenExpr a = QGenExpr { unQGenExpr :: Maybe Atom }

class SqlValable a where
    val_ :: HaskellLiteralForQExpr a -> a

instance (Atomable a) => SqlValable (QGenExpr a) where
  val_ = QGenExpr . Just . toAtom

instance (Beamable table, FieldsFulfillConstraint Atomable table)
  => SqlValable (table QGenExpr) where
  val_ tbl =
    let fields :: table (WithConstraint Atomable)
        fields = withConstrainedFields tbl
    in  changeBeamRep
          (\(Columnar' (WithConstraint x :: WithConstraint Atomable x)) ->
            Columnar' (QGenExpr (Just $ toAtom x))
          )
          fields

instance ( Beamable table, FieldsFulfillConstraintNullable Atomable table )
  => SqlValable (table (Nullable QGenExpr)) where

  val_ tbl =
    let fields :: table (Nullable (WithConstraint Atomable))
        fields = withNullableConstrainedFields tbl
    in  changeBeamRep
          (\(Columnar' (WithConstraint x :: WithConstraint Atomable (Maybe x))) ->
            Columnar' (QGenExpr (Just $ toAtom x))
          )
          fields
