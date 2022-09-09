{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
module: ProjectM36.Beamable.Operator
description: ProjectM36 implementation of Servant.Crud.Operator

Converts a 'table Filter' to ProjectM36 filters, so that this can be used in database queries

-}
module ProjectM36.Beamable.Operator
  ( matching_
  , Operator(..)
  , FieldsFulfillConstraintFilter
  , textAtomFuncs
  , (<&&>)
  ) where

import           Control.Monad.Writer           ( MonadWriter(..)
                                                , Writer
                                                , execWriter
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.Functor.Identity
import           Data.Kind                      ( Constraint )
import           Data.Monoid                    ( Last(..) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.String                    ( IsString(..) )
import           GHC.Generics                   ( Generic
                                                , K1(..)
                                                , R
                                                , Rep
                                                , to
                                                )
import           ProjectM36.AtomFunction        ( createScriptedAtomFunction )
import           ProjectM36.Atomable            ( Atomable(..) )
import           ProjectM36.Base
import           ProjectM36.Beamable.Class
import           ProjectM36.DataTypes.Primitive
import           ProjectM36.Shortcuts
import           Servant.Crud.QueryOperator


-- | This specifies how an operator @c@ with value type @a@ can be converted to
-- a filter in the ProjectM36 EDSL.
--
-- Instances are provided here for all the standard operators from "Servant.Crud.QueryOperator",
-- but if you have custom operators, this is where you need to write an instance for.
-- In this case write an instance for 'OpEntry operator valuetype'.
class Operator c a where
  passes :: c a  -- ^ The condition, in the spirit of \" > 5 \"
    -> AttributeName  -- ^ The value to match, e.g. 6, but then as a ProjectM36 Expression
    -> RestrictionPredicateExpr
    -- ^ The result of the match, e.g. True (since 6 > 5), but then as a ProjectM36 Expression.

instance Operator (OpDict '[]) a where
  passes Nil _ = TruePredicate

(<&&>)
  :: RestrictionPredicateExpr
  -> RestrictionPredicateExpr
  -> RestrictionPredicateExpr
TruePredicate <&&> y             = y
y             <&&> TruePredicate = y
x             <&&> y             = x `AndPredicate` y

instance (Operator (OpEntry s k) a, Operator (OpDict xs) a)
  => Operator (OpDict ( '(s, k) ': xs)) a where
  passes (x :> xs) y = passes x y <&&> passes xs y


instance Operator (OpDict (DefaultFilters a)) a => Operator Filter a where
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
  . zipBeamFieldsM go (withConstraintsFilter @c @tbl)
 where
  go
    :: forall a
     . Columnar' (HasConstraintFilter c) a
    -> Columnar' Filter a
    -> Identity (Columnar' (WithConstraintFilter c) a)
  go (Columnar' HasConstraintFilter) (Columnar' a) =
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
  :: forall table
   . (Table table, FieldsFulfillConstraintFilter (Operator Filter) table)
  => table Filter -- ^ The filters to match
  -> RestrictionPredicateExpr
matching_ tblFilter = foldl' (<&&>) trueP $ execWriter $ zipBeamFieldsM
  go
  (addConstraint tblFilter)
  (tblFieldSettings :: TableSettings table)
 where
  addConstraint
    :: table Filter -> table (WithConstraintFilter (Operator Filter))
  addConstraint = withConstrainedFieldsFilter

  go
    :: forall a
     . Columnar' (WithConstraintFilter (Operator Filter)) a
    -> Columnar' (TableField table) a
    -> Writer [RestrictionPredicateExpr] (Columnar' Proxy a)
  go (Columnar' (WithConstraintFilter cond)) (Columnar' val) = do
    tell [cond `passes` fieldAttributeName val]
    pure (Columnar' Proxy)

inP
  :: (Atomable a, Functor f, Foldable f)
  => AttributeName
  -> f a
  -> RestrictionPredicateExpr
inP y xs = foldl' (|||) falseP $ fmap (y ?=) xs

-- * Default instances
instance (Atomable a)
    => Operator (OpEntry "" 'List) a where
  passes (E []) _ = TruePredicate
  passes (E xs) y = inP y xs

instance (Atomable a)
    => Operator (OpEntry "!" 'List) a where
  passes (E []) _ = TruePredicate
  passes (E xs) y = NotPredicate $ inP y xs

isJust_ :: Atomable a => a -> RestrictionPredicateExpr
isJust_ a = AtomExprPredicate $ f "isJust" [a]

instance Operator (OpEntry "null" 'Flag) (Maybe a) where
  passes (E NotPresent) _ = TruePredicate
  passes (E Present   ) y = NotPredicate $ isJust_ y

instance Operator (OpEntry "!null" 'Flag) (Maybe a) where
  passes (E NotPresent) _ = TruePredicate
  passes (E Present   ) y = isJust_ y

-- | Helper function for passes function for Ord instances
passesOrd
  :: forall a sym
   . (Atomable a, Ord a)
  => FunctionName
  -> OpEntry sym 'Normal a
  -> AttributeName
  -> RestrictionPredicateExpr
passesOrd _    (E (Last Nothing )) _ = TruePredicate
passesOrd comp (E (Last (Just x))) y = AtomExprPredicate
  $ f comp [AttributeAtomExpr y, toAtomExpr . toAtom $ x]
 where
  _notused :: a -> a -> Bool
  _notused = (>)

instance (Atomable a, Ord a)
    => Operator (OpEntry "gt" 'Normal) a where
  passes = passesOrd "gt"

instance (Atomable a, Ord a)
    => Operator (OpEntry "lt" 'Normal) a where
  passes = passesOrd "lt"

instance (Atomable a, Ord a)
    => Operator (OpEntry "ge" 'Normal) a where
  passes = passesOrd "gte"

instance (Atomable a, Ord a)
    => Operator (OpEntry "le" 'Normal) a where
  passes = passesOrd "lte"

boolTypeConstructor :: TypeConstructor
boolTypeConstructor = PrimitiveTypeConstructor "Bool" BoolAtomType

textAtomFuncs :: [DatabaseContextIOExpr]
textAtomFuncs =
  let textBinOp n = createScriptedAtomFunction
        n
        [textTypeConstructor, textTypeConstructor]
        boolTypeConstructor
  in
    [ textBinOp
      "isPrefixOf"
      "(\\(TextAtom x: TextAtom y:_) -> pure (BoolAtom $ T.isPrefixOf (T.toCaseFold x) (T.toCaseFold y))) :: [Atom] -> Either AtomFunctionError Atom"
    , textBinOp
      "isSuffixOf"
      "(\\(TextAtom x: TextAtom y:_) -> pure (BoolAtom $ T.isSuffixOf (T.toCaseFold x) (T.toCaseFold y))) :: [Atom] -> Either AtomFunctionError Atom"
    , textBinOp
      "isInfixOf"
      "(\\(TextAtom x: TextAtom y:_) -> pure (BoolAtom $ T.isInfixOf (T.toCaseFold x) (T.toCaseFold y))) :: [Atom] -> Either AtomFunctionError Atom"
    , textBinOp
      "mIsPrefixOf"
      "(\\(TextAtom x: y:_) -> case y of \n\
        \  (ConstructedAtom \"Nothing\" _ _) -> pure (BoolAtom False)\n\
        \  (ConstructedAtom \"Just\" _ (TextAtom y:_)) -> pure (BoolAtom $ T.isPrefixOf (T.toCaseFold x) (T.toCaseFold y))\n\
        \  _ -> Left AtomFunctionTypeMismatchError) :: [Atom] -> Either AtomFunctionError Atom"
    , textBinOp
      "mIsSuffixOf"
      "(\\(TextAtom x: y:_) -> case y of \n\
        \  (ConstructedAtom \"Nothing\" _ _) -> pure (BoolAtom False)\n\
        \  (ConstructedAtom \"Just\" _ (TextAtom y:_)) -> pure (BoolAtom $ T.isSuffixOf (T.toCaseFold x) (T.toCaseFold y))\n\
        \  _ -> Left AtomFunctionTypeMismatchError) :: [Atom] -> Either AtomFunctionError Atom"
    , textBinOp
      "mIsInfixOf"
      "(\\(TextAtom x: y:_) -> case y of \n\
        \  (ConstructedAtom \"Nothing\" _ _) -> pure (BoolAtom False)\n\
        \  (ConstructedAtom \"Just\" _ (TextAtom y:_)) -> pure (BoolAtom $ T.isInfixOf (T.toCaseFold x) (T.toCaseFold y))\n\
        \  _ -> Left AtomFunctionTypeMismatchError) :: [Atom] -> Either AtomFunctionError Atom"
    -- TODO write isMatchOf (probably using T.breakOn and T.splitOn and T.null)
    ]

-- | Helper for positive string tests
passesStr
  :: forall a sym
   . (IsString a, Atomable a)
  => FunctionName
  -> OpEntry sym 'Normal a
  -> AttributeName
  -> RestrictionPredicateExpr
passesStr _ (E (Last Nothing )) _ = TruePredicate
passesStr n (E (Last (Just x))) y = AtomExprPredicate
  $ f n [toAtomExpr . toAtom $ x, AttributeAtomExpr y]
 where
  _notused :: String -> a
  _notused = fromString


-- | Helper for negative string tests
passesStrN
  :: (IsString a, Atomable a)
  => FunctionName
  -> OpEntry sym 'Normal a
  -> AttributeName
  -> RestrictionPredicateExpr
passesStrN _ (E (Last Nothing)) _ = TruePredicate
passesStrN n x                  y = NotPredicate $ passesStr n x y

instance ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "start" 'Normal) a where
  passes = passesStr "isPrefixOf"

instance ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "!start" 'Normal) a where
  passes = passesStrN "isPrefixOf"

instance ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "end" 'Normal) a where
  passes = passesStr "isSuffixOf"

instance ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "!end" 'Normal) a where
  passes = passesStrN "isSuffixOf"

instance ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "contains" 'Normal) a where
  passes = passesStr "isInfixOf"

instance ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "!contains" 'Normal) a where
  passes = passesStrN "isInfixOf"

instance ( IsString a
         , Atomable a)
    => Operator (OpEntry "match" 'Normal) a where
  passes = passesStr "isMatchOf"

instance ( IsString a
         , Atomable a)
    => Operator (OpEntry "!match" 'Normal) a where
  passes = passesStrN "isMatchOf"



-- Variants for Maybe
-- | Helper for positive string tests with nullable fields
passesStrMaybe
  :: forall a sym
   . (IsString a, Atomable a)
  => FunctionName
  -> OpEntry sym 'Normal (Maybe a)
  -> AttributeName
  -> RestrictionPredicateExpr
passesStrMaybe _ (E (Last Nothing        )) _ = TruePredicate
passesStrMaybe _ (E (Last (Just Nothing ))) _ = TruePredicate
passesStrMaybe n (E (Last (Just (Just x)))) y = AtomExprPredicate
  $ f n [toAtomExpr . toAtom $ x, AttributeAtomExpr y]
 where
  _notused :: String -> a
  _notused = fromString

-- | Helper for positive string tests with nullable fields
passesStrMaybeN
  :: (IsString a, Atomable a)
  => FunctionName
  -> OpEntry sym 'Normal (Maybe a)
  -> AttributeName
  -> RestrictionPredicateExpr
passesStrMaybeN _ (E (Last Nothing)) _ = TruePredicate
passesStrMaybeN _ (E (Last (Just Nothing))) _ = TruePredicate
passesStrMaybeN n x y = NotPredicate $ passesStrMaybe n x y

instance {-# OVERLAPPING  #-} ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "start" 'Normal) (Maybe a) where
  passes = passesStrMaybe "mIsPrefixOf"

instance {-# OVERLAPPING  #-} ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "!start" 'Normal) (Maybe a) where
  passes = passesStrMaybeN "mIsPrefixOf"

instance {-# OVERLAPPING  #-} ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "end" 'Normal) (Maybe a) where
  passes = passesStrMaybe "mIsSuffixOf"

instance {-# OVERLAPPING  #-} ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "!end" 'Normal) (Maybe a) where
  passes = passesStrMaybeN "mIsSuffixOf"

instance {-# OVERLAPPING  #-} ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "contains" 'Normal) (Maybe a) where
  passes = passesStrMaybe "mIsInfixOf"

instance {-# OVERLAPPING  #-} ( IsString a
         , Atomable a
         )
    => Operator (OpEntry "!contains" 'Normal) (Maybe a) where
  passes = passesStrMaybeN "mIsInfixOf"

instance {-# OVERLAPPING  #-} ( IsString a
         , Atomable a)
    => Operator (OpEntry "match" 'Normal) (Maybe a) where
  passes = passesStrMaybe "mIsMatchOf"

instance {-# OVERLAPPING  #-} ( IsString a
         , Atomable a)
    => Operator (OpEntry "!match" 'Normal) (Maybe a) where
  passes = passesStrMaybeN "mIsMatchOf"
