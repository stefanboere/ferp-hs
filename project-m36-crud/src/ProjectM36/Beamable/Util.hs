{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ProjectM36.Beamable.Util
  ( makePatch
  , purePatch
  , joinPatch
  , ignorePrimary
  , allBeamValues
  ) where

import           Control.Applicative            ( Const(..) )
import           Control.Monad.State            ( MonadState(..)
                                                , evalState
                                                )
import           Control.Monad.Writer           ( MonadWriter(..)
                                                , Writer
                                                , execWriter
                                                )
import           Data.Functor.Identity
import           Data.Monoid                    ( Last(..) )
import           Data.Proxy                     ( Proxy(..) )
import           ProjectM36.Beamable.Class


-- | Calculat the difference between two objects
makePatch
  :: forall t
   . (FieldsFulfillConstraint Eq t, Beamable t)
  => t Identity
  -> t Identity
  -> t Last
makePatch old new = runIdentity $ zipBeamFieldsM
  (\(Columnar' colOld) (Columnar' (WithConstraint colNew)) ->
    pure . Columnar' . Last $ if colOld == colNew then Nothing else Just colNew
  )
  old
  (withConstrainedFields new :: t (WithConstraint Eq))

-- | Create a patch that sets all columns to a  new value
purePatch :: forall t . Beamable t => t Identity -> t Last
purePatch x = runIdentity $ zipBeamFieldsM
  (\_ (Columnar' colNew) -> pure . Columnar' . Last $ Just colNew)
  (tblSkeleton :: t Proxy)
  x

-- | Move the maybe thought the table structure.
-- Returns Just if all fields are Just.
joinPatch :: forall t . Beamable t => t Last -> Maybe (t Identity)
joinPatch = zipBeamFieldsM
  (\_ (Columnar' (Last colNew)) -> fmap Columnar' colNew)
  (tblSkeleton :: t Proxy)

-- | Set the expression for the primary key back to the default.
--
-- This means that you can just enter some value in the primary key field without
-- worrying that this id might already exist, as the value you provided there will
-- be skipped. This way you can insert values of type @table Identity@.
ignorePrimary
  :: forall table . (Table table) => table QGenExpr -> table QGenExpr
ignorePrimary tbl = runIdentity $ zipBeamFieldsM
  (\(Columnar' (Const columnIx)) (Columnar' (QGenExpr val)) ->
    pure $ Columnar' $ if columnIx `elem` primaryKeyIndices
      then QGenExpr Nothing
      else QGenExpr val
  )
  indexedTable
  tbl

 where
  indexedTable :: table (Const Int)
  indexedTable = flip evalState 0 $ zipBeamFieldsM
    (\_ (Columnar' _) -> do
      n <- get
      put (n + 1)
      return (Columnar' (Const n))
    )
    (tblSkeleton :: table Proxy)
    tbl

  primaryKeyIndices :: [Int]
  primaryKeyIndices =
    allBeamValues (\(Columnar' (Const ix)) -> ix) (primaryKey indexedTable)


allBeamValues
  :: Beamable table => (forall a . Columnar' f a -> b) -> table f -> [b]
allBeamValues (f :: forall a . Columnar' f a -> b) (tbl :: table f) =
  execWriter (zipBeamFieldsM combine tbl tbl)
 where
  combine :: Columnar' f a -> Columnar' f a -> Writer [b] (Columnar' f a)
  combine x _ = do
    tell [f x]
    return x
