{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module: Database.Beam.Extra
Description: Extensions to the Beam EDSL

This module is mostly about working with primary keys more easily.

-}
module Database.Beam.Extra
  ( -- * Selecting
    runLookupIn
    -- * Updating
  , patch
  , runPatch
  , save'
  , runSave'
  , saveWithId
  , runSaveWithId
    -- * Inserting
  , insertValues'
  , runInsertOne
  , runInsertMany
    -- * Deleting
  , runDeleteKey
  , runDeleteKeys
    -- * Utilities
  , ignorePrimary
  , countIn_
  , runCountIn
  , runSelectReturningListWithCount
  , runIsInView
  , makePatch
  , purePatch
  , joinPatch
  , FieldsFulfillConstraint
  ) where

import           Prelude

import           Control.Applicative            ( Const(..) )
import           Control.Monad.Trans.State.Lazy ( evalState
                                                , get
                                                , put
                                                )
import           Data.Functor.Identity
import           Data.Maybe                     ( isJust )
import           Database.Beam           hiding ( save' )
import           Database.Beam.Backend.SQL      ( BeamSql2003ExpressionBackend
                                                , BeamSqlBackend
                                                , BeamSqlBackendCanSerialize
                                                )
import           Database.Beam.Backend.SQL.BeamExtensions
                                                ( MonadBeamInsertReturning
                                                , runInsertReturningList
                                                )
import           Database.Beam.Query.Internal
import           Database.Beam.Schema.Tables
import           Servant.Crud.QueryOperator     ( MaybeLast(..) )

-- | Saves all 'Just' fields in the database. This is useful for PATCH requests.
--
-- For example, this is how you update the price of a book while keeping every thing else fixed.
--
-- > bookPatch :: BookT Maybe
-- > bookPatch = Book { price = Just (Dollar 10), author = Nothing, id = Nothing, ... }
-- >
-- > setPriceToTenDollars :: (..) => DatabaseEntity be db (TableEntity BookT) -> PrimaryKey BookT Identity -> SqlUpdate be BookT
-- > setPriceToTenDollars db id = patch db id bookPatch
patch
  :: forall t be db
   . ( Table t
     , BeamSqlBackend be
     , FieldsFulfillConstraintNullable (BeamSqlBackendCanSerialize be) t
     , FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey t)
     , SqlValableTable be (PrimaryKey t)
     )
  => DatabaseEntity be db (TableEntity t)
        -- ^ Table to update
  -> PrimaryKey t Identity
        -- ^ Primary key of the row
  -> t MaybeLast
        -- ^ Value to set possibly, primary keys are ignored
  -> SqlUpdate be t
patch table key v = updateTable
  table
  (setFieldsToMaybe v (val_ (changeBeamRep toNullable v)))
  (references_ (val_ key))
 where
  toNullable :: Columnar' MaybeLast a -> Columnar' (Nullable Identity) a
  toNullable ~(Columnar' (MaybeLast x)) = Columnar' x

  setFieldsToMaybe
    :: forall table be' table'
     . (Table table)
    => table MaybeLast
    -> forall s
     . table (Nullable (QExpr be' s))
    -> table (QFieldAssignment be' table')
  setFieldsToMaybe tbl tblExpr = runIdentity $ zipBeamFieldsM
    (\(Columnar' (Const (columnIx, isSet))) (Columnar' (QExpr newValue)) ->
      pure $ Columnar' $ if columnIx `elem` primaryKeyIndices || not isSet
        then toOldValue
        else toNewValue (QExpr newValue)
    )
    indexedTable
    tblExpr

   where
    indexedTable :: table (Const (Int, Bool))
    indexedTable = flip evalState 0 $ zipBeamFieldsM
      (\_ (Columnar' x) -> do
        n <- get
        put (n + 1)
        return (Columnar' (Const (n, isJust $ unMaybeLast x)))
      )
      (tblSkeleton :: TableSkeleton table)
      tbl

    primaryKeyIndices :: [Int]
    primaryKeyIndices = allBeamValues (\(Columnar' (Const (ix, _))) -> ix)
                                      (primaryKey indexedTable)

-- | Run 'patch' inside a BeamMonad
runPatch
  :: forall t db be m
   . ( Table t
     , BeamSqlBackend be
     , MonadBeam be m
     , FieldsFulfillConstraintNullable (BeamSqlBackendCanSerialize be) t
     , FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey t)
     , SqlValableTable be (PrimaryKey t)
     )
  => DatabaseEntity be db (TableEntity t)
        -- ^ Table to update
  -> PrimaryKey t Identity
        -- ^ Primary key of the row
  -> t MaybeLast
        -- ^ Value to set possibly
  -> m ()
runPatch tbl key v = runUpdate $ patch tbl key v

-- | Calculat the difference between two objects
makePatch
  :: forall t
   . (FieldsFulfillConstraint Eq t, Beamable t)
  => t Identity
  -> t Identity
  -> t MaybeLast
makePatch old new = runIdentity $ zipBeamFieldsM
  (\(Columnar' colOld) (Columnar' (WithConstraint colNew)) ->
    pure . Columnar' . MaybeLast $ if colOld == colNew
      then Nothing
      else Just colNew
  )
  old
  (withConstrainedFields @Eq new)

-- | Create a patch that sets all columns to a  new value
purePatch :: forall t . Beamable t => t Identity -> t MaybeLast
purePatch x = runIdentity $ zipBeamFieldsM
  (\_ (Columnar' colNew) -> pure . Columnar' . MaybeLast $ Just colNew)
  (tblSkeleton :: TableSkeleton t)
  x

-- | Move the maybe thought the table structure.
-- Returns Just if all fields are Just.
joinPatch :: forall t . Beamable t => t MaybeLast -> Maybe (t Identity)
joinPatch = zipBeamFieldsM
  (\_ (Columnar' (MaybeLast colNew)) -> fmap Columnar' colNew)
  (tblSkeleton :: TableSkeleton t)


-- | Delete a row using the primary key
runDeleteKey
  :: forall db t be m
   . ( Table t
     , BeamSqlBackend be
     , MonadBeam be m
     , SqlValableTable be (PrimaryKey t)
     , FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey t)
     )
  => DatabaseEntity be db (TableEntity t)
  -> PrimaryKey t Identity
  -> m ()
runDeleteKey tbl key = runDelete $ delete tbl (references_ (val_ key))

-- | Delete rows using the primary key
runDeleteKeys
  :: forall db t be m
   . ( Table t
     , BeamSqlBackend be
     , MonadBeam be m
     , SqlValableTable be (PrimaryKey t)
     , HasSqlInTable be
     )
  => DatabaseEntity be db (TableEntity t)
  -> [PrimaryKey t Identity]
  -> m ()
runDeleteKeys tbl keys =
  runDelete $ delete tbl (\t -> pk t `in_` fmap val_ keys)

-- | Generate a 'SqlUpdate' that will update the given table row with the given value.
--
--   Like 'Database.Beam.Query.save', but does not update the primary keys.
save'
  :: forall table be db
   . ( Table table
     , BeamSqlBackend be
     , SqlValableTable be (PrimaryKey table)
     , SqlValableTable be table
     , HasTableEquality be (PrimaryKey table)
     )
  => DatabaseEntity be db (TableEntity table)
        -- ^ Table to update
  -> table Identity
        -- ^ Value to set to
  -> SqlUpdate be table
save' tbl v = updateTableRow tbl v (setFieldsTo (ignorePrimary $ val_ v))


-- | Save a value to the table without returning it. Does not update the primary key.
runSave'
  :: forall t db be m
   . ( Table t
     , BeamSqlBackend be
     , MonadBeam be m
     , SqlValableTable be (PrimaryKey t)
     , SqlValableTable be t
     , FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey t)
     )
  => DatabaseEntity be db (TableEntity t)
       -- ^ Table to update
  -> t Identity
       -- ^ Value to update
  -> m ()
runSave' tbl v = runUpdate $ save' tbl v


-- | Like 'save'', but explicitely pass the primary key.
saveWithId
  :: forall table be db
   . ( Table table
     , BeamSqlBackend be
     , SqlValableTable be (PrimaryKey table)
     , SqlValableTable be table
     , HasTableEquality be (PrimaryKey table)
     )
  => DatabaseEntity be db (TableEntity table)  -- ^ Table to update
  -> PrimaryKey table Identity -- ^ Use this primary key, even though @t Identity@ may have a different id
  -> table Identity  -- ^ Value to set to
  -> SqlUpdate be table
saveWithId tbl key v = updateTable tbl
                                   (setFieldsTo (ignorePrimary $ val_ v))
                                   (references_ (val_ key))

-- | Like 'runSave'', but explictely pass the primary key
runSaveWithId
  :: forall t db be m
   . ( Table t
     , BeamSqlBackend be
     , MonadBeam be m
     , SqlValableTable be (PrimaryKey t)
     , SqlValableTable be t
     , FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey t)
     )
  => DatabaseEntity be db (TableEntity t) -- ^ Table to update
  -> PrimaryKey t Identity -- ^ Use this primary key, even though @t Identity@ may have a different id
  -> t Identity -- ^ Value to update
  -> m ()
runSaveWithId tbl key v = runUpdate $ saveWithId tbl key v

-- | Inserts a value and returns the primary key of the newly created value.
--
-- Returns 'Nothing' if the insert failed.
runInsertOne
  :: forall t db be m
   . ( BeamSqlBackend be
     , Table t
     , MonadBeam be m
     , MonadBeamInsertReturning be m
     , FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) t
     , FromBackendRow be (t Identity)
     )
  => DatabaseEntity be db (TableEntity t)
      -- ^ Table to update
  -> t Identity
      -- ^ Value to insert
  -> m (Maybe (PrimaryKey t Identity))
runInsertOne tbl v = headMaybe <$> runInsertMany tbl [v]
 where
  headMaybe :: [a] -> Maybe a
  headMaybe []      = Nothing
  headMaybe (x : _) = Just x

-- | Inserts a value and returns the primary key of the newly created value.
--
-- Returns 'Nothing' if the insert failed.
runInsertMany
  :: forall t db be m
   . ( BeamSqlBackend be
     , Table t
     , MonadBeam be m
     , MonadBeamInsertReturning be m
     , FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) t
     , FromBackendRow be (t Identity)
     )
  => DatabaseEntity be db (TableEntity t)
      -- ^ Table to update
  -> [t Identity]
      -- ^ Values to insert
  -> m [PrimaryKey t Identity]
runInsertMany tbl v = do
  inserted <- runInsertReturningList $ insert tbl $ insertValues' v
  pure $ fmap primaryKey inserted

-- | Build a 'SqlInsertValues' from concrete table values, but ignores the primary keys.
insertValues'
  :: forall be table s
   . ( BeamSqlBackend be
     , Table table
     , FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) table
     )
  => [table Identity]
  -> SqlInsertValues be (table (QExpr be s))
insertValues' x = insertExpressions
  (map (ignorePrimary . val_) x :: forall s' . [table (QExpr be s')])

-- | Set the expression for the primary key back to the default.
--
-- This means that you can just enter some value in the primary key field without
-- worrying that this id might already exist, as the value you provided there will
-- be skipped. This way you can insert values of type @table Identity@.
ignorePrimary
  :: forall table be s
   . (BeamSqlBackend be, Table table)
  => table (QExpr be s)
  -> table (QExpr be s)
ignorePrimary tbl = runIdentity $ zipBeamFieldsM
  (\(Columnar' (Const columnIx)) (Columnar' (QExpr val)) ->
    pure $ Columnar' $ if columnIx `elem` primaryKeyIndices
      then default_
      else QExpr val
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
    (tblSkeleton :: TableSkeleton table)
    tbl

  primaryKeyIndices :: [Int]
  primaryKeyIndices =
    allBeamValues (\(Columnar' (Const ix)) -> ix) (primaryKey indexedTable)

-- | Counts the number of rows in the supplied expression, i.e. the SQL @COUNT(*)@.
countIn_
  :: (BeamSqlBackend be, Projectible be r, Integral a)
  => Q be db (QNested s) r
  -> Q
       be
       db
       s
       ( WithRewrittenThread
           (QNested s)
           s
           (WithRewrittenContext (QAgg be (QNested s) a) QValueContext)
       )
countIn_ = aggregate_ (const countAll_)


-- | Runs 'countIn_'  in a 'MonadBeam'
runCountIn
  :: ( Projectible be r
     , MonadBeam be m
     , HasQBuilder be
     , FromBackendRow be a
     , Integral a
     )
  => Q be db (QNested QBaseScope) r
  -> m (Maybe a)
runCountIn = runSelectReturningOne . select . countIn_

runSelectReturningListWithCount
  :: ( Projectible
         be
         ( WithRewrittenThread
             (QNested QBaseScope)
             QBaseScope
             (WithRewrittenContext a QValueContext)
         )
     , Projectible be a
     , MonadBeam be m
     , HasQBuilder be
     , FromBackendRow
         be
         ( QExprToIdentity
             ( WithRewrittenThread
                 (QNested QBaseScope)
                 QBaseScope
                 (WithRewrittenContext a QValueContext)
             )
         )
     , FromBackendRow be Integer
     , Integral c
     , BeamSql2003ExpressionBackend be
     , ContextRewritable a
     , ThreadRewritable
         (QNested QBaseScope)
         (WithRewrittenContext a QValueContext)
     )
  => Q be db (QNested QBaseScope) a
  -> m
       ( [ QExprToIdentity
             ( WithRewrittenThread
                 (QNested QBaseScope)
                 QBaseScope
                 (WithRewrittenContext a QValueContext)
             )
         ]
       , c
       )
runSelectReturningListWithCount q = do
  rs <- runSelectReturningList $ select $ withWindow_
    (\_ -> frame_ (noPartition_ @Integer) (noOrder_ @Integer) noBounds_)
    (\i r -> (i, countAll_ `over_` r))
    q
  let c = case rs of
        []         -> 0
        (_, x) : _ -> fromInteger x
  pure (fmap fst rs, c)


-- | Find a row in the collection matching the id
runLookupIn
  :: ( BeamSqlBackend be
     , HasQBuilder be
     , Table t
     , MonadBeam be m
     , FromBackendRow be (t Identity)
     , SqlValableTable be (PrimaryKey t)
     , HasTableEquality be (PrimaryKey t)
     )
  => PrimaryKey t Identity -- ^ The id
  -> Q be db QBaseScope (t (QExpr be QBaseScope)) -- ^ The collection
  -> m (Maybe (t Identity))
runLookupIn tblKey tbl =
  runSelectReturningOne $ select $ filter_ (references_ (val_ tblKey)) tbl

-- | Whether a row with some id belongs to the collection
runIsInView
  :: ( BeamSqlBackend be
     , HasQBuilder be
     , Table t
     , MonadBeam be m
     , FromBackendRow be (PrimaryKey t Identity)
     , SqlValableTable be (PrimaryKey t)
     , HasTableEquality be (PrimaryKey t)
     )
  => PrimaryKey t Identity -- ^ The id
  -> Q be db QBaseScope (t (QExpr be QBaseScope)) -- ^ The collection
  -> m Bool
runIsInView tblKey tbl = isJust <$> runSelectReturningOne
  (select $ do
    row <- filter_ (references_ (val_ tblKey)) tbl
    pure $ pk row
  )
