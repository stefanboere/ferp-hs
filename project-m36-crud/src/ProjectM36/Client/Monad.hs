{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ProjectM36.Client.Monad
  ( Db(..)
  , MonadM36
  , DbError(..)
  , DbConn
  , simpleConnectProjectM36
  , simpleConnectProjectM36At
  , close
  , withTransaction
  , withTransactionUsing
  , query
  , execute
  , executeIO
  , C.RelationalError
  , C.ConnectionInfo(..)
  , C.emptyNotificationCallback
    -- * Client modifying functions
  , runDeleteKey
  , runDeleteKeys
  , runInsertMany
  , runInsertOne
  , runPatch
  , runSave
  , defineRelations
  , defineDatabase
  -- * Query function
  , runLookupIn
  , runIsInView
  , runSelectReturningList
  , runCountIn
  , runSetView
  , runSetViewAround
  , runPrimaryKeysInView
  ) where

import           Control.Monad.Except           ( ExceptT(..)
                                                , MonadError(..)
                                                , liftEither
                                                , runExceptT
                                                )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Control.Monad.Writer
import           Data.Either                    ( partitionEithers )
import           Data.Functor.Identity          ( Identity )
import           Data.List                      ( partition )
import           Data.Maybe                     ( isJust
                                                , listToMaybe
                                                )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Set                      as Set
import           ProjectM36.Atomable            ( Atomable )
import           ProjectM36.Base
import qualified ProjectM36.Client             as C
import           ProjectM36.DataFrame           ( AttributeOrderExpr
                                                , DataFrameExpr(..)
                                                , DataFrameTuple(..)
                                                , tuples
                                                )
import           ProjectM36.DataTypes.Primitive ( atomTypeForAtom )
import qualified ProjectM36.Relation           as Relation
import           ProjectM36.Relation            ( tuplesList )
import           ProjectM36.Tuple               ( atomForAttributeName )
import           Servant.Crud.Headers           ( ExceptLimited(..) )

import           ProjectM36.Beamable

type DbConn = (C.SessionId, C.Connection)
type MonadM36 m
  = (MonadError C.RelationalError m, MonadReader DbConn m, MonadIO m)

newtype Db a = Db {runDb :: ExceptT C.RelationalError (ReaderT DbConn IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader DbConn, MonadError C.RelationalError)

data DbError = ConnError C.ConnectionError
             | RelError C.RelationalError
             deriving stock (Eq, Show)

-- | A simple alternative to 'connectProjectM36' which includes simple session management.
simpleConnectProjectM36At
  :: C.HeadName -> C.ConnectionInfo -> IO (Either DbError DbConn)
simpleConnectProjectM36At headName connInfo = do
  eConn <- C.connectProjectM36 connInfo
  case eConn of
    Left  err  -> pure (Left (ConnError err))
    Right conn -> do
      eSess <- C.createSessionAtHead conn headName
      case eSess of
        Left err -> do
          C.close conn
          pure (Left (RelError err))
        Right sess -> pure (Right (sess, conn))

-- | Same as 'simpleConnectProjectM36At' but always connects to the @master@ branch.
simpleConnectProjectM36 :: C.ConnectionInfo -> IO (Either DbError DbConn)
simpleConnectProjectM36 = simpleConnectProjectM36At "master"

-- | Closes the database connection.
close :: DbConn -> IO ()
close (_, conn) = C.close conn

-- | Runs a Db monad which may include some database updates. If an exception or error occurs, the transaction is rolled back. Otherwise, the transaction is committed to the head of the current branch.
withTransaction :: DbConn -> Db a -> IO (Either C.RelationalError a)
withTransaction sessconn = withTransactionUsing sessconn C.UnionMergeStrategy

-- | Same as 'withTransaction' except that the merge strategy can be specified.
withTransactionUsing
  :: DbConn -> C.MergeStrategy -> Db a -> IO (Either C.RelationalError a)
withTransactionUsing (sess, conn) strat dbm = do
  eHeadName <- C.headName sess conn
  case eHeadName of
    Left  err      -> pure (Left err)
    Right headName -> do
      let successFunc = C.autoMergeToHead sess conn strat headName
          block       = runReaderT (runExceptT $ runDb dbm) (sess, conn)
      C.withTransaction sess conn block successFunc

-- | Execute a 'a' in the 'DB' monad. Database context expressions manipulate the state of the database. In case of an error, the transaction is terminated and the connection's session is rolled back.
execute'
  :: MonadM36 m
  => (C.SessionId -> C.Connection -> a -> IO (Either C.RelationalError b))
  -> a
  -> m b
execute' fn expr = do
  (sess, conn) <- ask
  r            <- liftIO $ fn sess conn expr
  liftEither r

-- | Execute a 'DatabaseContextExpr' in the 'DB' monad. Database context expressions manipulate the state of the database. In case of an error, the transaction is terminated and the connection's session is rolled back.
execute :: MonadM36 m => C.DatabaseContextExpr -> m ()
execute = execute' C.executeDatabaseContextExpr

-- | Execute a 'DatabaseContextIOExpr' in the 'DB' monad. Database context expressions manipulate the state of the database. In case of an error, the transaction is terminated and the connection's session is rolled back.
executeIO :: MonadM36 m => C.DatabaseContextIOExpr -> m ()
executeIO = execute' C.executeDatabaseContextIOExpr

-- | Run a 'RelationalExpr' query in the 'DB' monad. Relational expressions perform read-only queries against the current database state.
query :: MonadM36 m => C.RelationalExpr -> m Relation
query = execute' C.executeRelationalExpr


class IsTable proxyt where
  dbDefineExpr :: proxyt -> DatabaseContextExpr

instance (Table t, FieldsFulfillConstraint Atomable t) => IsTable (Proxy t) where
  dbDefineExpr = toDefineExpr

defineDatabase
  :: forall db m
   . (Beamable db, FieldsFulfillConstraint IsTable db, MonadM36 m)
  => db Identity
  -> m ()
defineDatabase db =
  let ds = execWriter $ zipBeamFieldsM
        f
        tblSkeleton
        (withConstrainedFields db :: db (WithConstraint IsTable))
  in  do
        conn <- ask
        r    <- liftIO $ defineRelations conn ds
        liftEither r
 where
  f
    :: forall a
     . Columnar' Proxy a
    -> Columnar' (WithConstraint IsTable) a
    -> Writer [DatabaseContextExpr] (Columnar' Proxy a)
  f _ (Columnar' (WithConstraint x)) = do
    tell [dbDefineExpr x]
    pure $ Columnar' Proxy

defineRelations
  :: DbConn -> [DatabaseContextExpr] -> IO (Either C.RelationalError ())
defineRelations dbconn ds = do
  rels <- readColumn "name" <$> uncurry C.relationVariablesAsRelation dbconn
  typs <- readColumn "TypeConstructor" <$> uncurry C.atomTypesAsRelation dbconn
  case (,) <$> rels <*> typs of
    Left  err      -> pure (Left err)
    Right (rs, ts) -> do
      let ds' = removeExisting (Set.fromList rs) (Set.fromList ts) ds
      uncurry C.executeDatabaseContextExpr
              dbconn
              (MultipleExpr (typeConsFirst ds'))

 where
  readColumn col xs =
    (mapM (atomForAttributeName col >=> attrName) . tuplesList) =<< xs
  attrName (TextAtom a) = Right a
  attrName atom =
    Left (C.AtomTypeMismatchError TextAtomType (atomTypeForAtom atom))

  removeExisting _  _  []                     = []
  removeExisting rs ts (MultipleExpr xs : ys) = removeExisting rs ts (xs ++ ys)
  removeExisting rs ts (xx@(Define x _) : ys)
    | x `Set.member` rs = ys
    | otherwise         = xx : removeExisting (Set.insert x rs) ts ys
  removeExisting rs ts (xx@(AddTypeConstructor (ADTypeConstructorDef x _) _) : ys)
    | x `Set.member` ts
    = ys
    | otherwise
    = xx : removeExisting rs (Set.insert x ts) ys
  removeExisting rs ts (x : ys) = x : removeExisting rs ts ys

  typeConsFirst xs = let (x0, x1) = partition isTypeCon xs in x0 ++ x1
  isTypeCon (AddTypeConstructor _ _) = True
  isTypeCon _                        = False

runPatch
  :: ( Table t
     , FieldsFulfillConstraintNullable Atomable t
     , FieldsFulfillConstraint Atomable (PrimaryKey t)
     , MonadM36 m
     )
  => PrimaryKey t Identity
  -> t Last
  -> m ()
runPatch pk v = do
  let x = toPatchExpr pk v
  liftIO $ print x
  execute x

runDeleteKey
  :: (Table t, FieldsFulfillConstraint Atomable (PrimaryKey t), MonadM36 m)
  => PrimaryKey t Identity
  -> m ()
runDeleteKey = execute . toDeleteExpr

runDeleteKeys
  :: (Table t, FieldsFulfillConstraint Atomable (PrimaryKey t), MonadM36 m)
  => [PrimaryKey t Identity]
  -> m ()
runDeleteKeys = execute . toDeletesExpr

runSave
  :: ( Table t
     , FieldsFulfillConstraint Atomable t
     , FieldsFulfillConstraint Atomable (PrimaryKey t)
     , MonadM36 m
     )
  => PrimaryKey t Identity
  -> t Identity
  -> m ()
runSave pk = execute . toSaveExpr pk

runInsertOne
  :: (Table t, FieldsFulfillConstraint Atomable t, MonadM36 m)
  => t Identity
  -> m ()
runInsertOne = runInsertMany . pure

-- TODO should check they do not exist yet, or else it can be a security issue
runInsertMany
  :: (Table t, FieldsFulfillConstraint Atomable t, MonadM36 m)
  => [t Identity]
  -> m ()
runInsertMany = execute . toInsertExpr

runSelectReturningList
  :: forall t n m
   . (IsView n t, MonadM36 m)
  => Proxy n
  -> Proxy t
  -> [AttributeOrderExpr]
  -> Maybe Integer
  -> Maybe Integer
  -> RestrictionPredicateExpr
  -> m [D n t Identity]
runSelectReturningList p pt ord off lim filt =
  let df = DataFrameExpr
        { convertExpr                 = C.Restrict filt (dbRelationalExpr p pt)
        , orderExprs                  = ord
        , ProjectM36.DataFrame.offset = off
        , ProjectM36.DataFrame.limit  = lim
        }
  in  do
        (sid, conn) <- ask
        rels        <- liftIO $ C.executeDataFrameExpr sid conn df
        x'          <- either throwError (pure . tuples') rels
        let (ls, rs) = partitionEithers $ fmap (dbFromTuple p pt) x'
        unless (null ls) (throwError (C.MultipleErrors ls))
        pure rs
 where
  tuples' =
    map (\(DataFrameTuple attrs' tupVec) -> RelationTuple attrs' tupVec)
      . tuples

runCountIn
  :: forall t n m c
   . (IsView n t, Table (BaseTable t), MonadM36 m, Integral c)
  => Proxy n
  -> Proxy t
  -> RestrictionPredicateExpr
  -> m (Maybe c)
runCountIn p pt filt = do
  rel <-
    query
    $ C.Project (C.AttributeNames $ primaryKeyAttributeNames pt')
    $ C.Restrict filt (dbRelationalExpr p pt)
  pure $ case Relation.cardinality rel of
    Finite x  -> Just $ fromIntegral x
    Countable -> Nothing
 where
  pt' :: Proxy (BaseTable t)
  pt' = Proxy

runLookupIn
  :: forall t n m
   . ( IsView n t
     , Table (BaseTable t)
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , MonadM36 m
     )
  => Proxy n
  -> Proxy t
  -> PrimaryKey (BaseTable t) Identity
  -> m (Maybe (D n t Identity))
runLookupIn p pt key = do
  rel <- query $ C.Restrict (references_ TruePredicate key) $ dbRelationalExpr
    p
    pt
  vals <- liftIO $ Relation.toList rel
  pure (listToMaybe vals >>= either (const Nothing) Just . dbFromTuple p pt)

runIsInView
  :: forall m t
   . ( IsView PrimaryKey t
     , Table (BaseTable t)
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , MonadM36 m
     )
  => Proxy t
  -> PrimaryKey (BaseTable t) Identity
  -> m Bool
runIsInView pt key = do
  v <- runLookupIn (Proxy :: Proxy PrimaryKey) pt key
  pure $ isJust v


-- | Helper function for implementing the deletelist route
runPrimaryKeysInView
  :: forall t m
   . ( Table (BaseTable t)
     , Table t
     , MonadM36 m
     , FieldsFulfillConstraintFilter (Operator Filter) t
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , IsView PrimaryKey t
     )
  => t Filter
  -> ExceptLimited [PrimaryKey (BaseTable t) Identity]
  -> m [PrimaryKey (BaseTable t) Identity]
runPrimaryKeysInView fltr ekeys = case ekeys of
  Except    xs -> go NotPredicate xs
  LimitedTo xs -> go id xs
 where
  go fn keys = runSelectReturningList
    (Proxy :: Proxy PrimaryKey)
    (Proxy :: Proxy t)
    []
    Nothing
    Nothing
    (fn (referencesAny_ keys) <&&> matching_ fltr)


runSetView
  :: forall n t m
   . ( Table t
     , IsView n t
     , FieldsFulfillConstraintFilter (Operator Filter) t
     , MonadM36 m
     )
  => Proxy n
  -> View t
  -> m [D n t Identity]
runSetView pn (View pag ords filt) = runSelectReturningList
  pn
  (Proxy :: Proxy t)
  (orderByO_ ords)
  (ProjectM36.Beamable.offset pag)
  (ProjectM36.Beamable.limit pag)
  (matching_ filt)

-- TODO implement
-- - Divide page size by two
-- - Do someting with the offset (negate and add)
-- - Convert sort to an additional filtering that filters away all rows coming before the current one
-- - Same in reverse direction
-- - Combine
runSetViewAround
  :: forall n t m
   . Proxy n
  -> PrimaryKey (BaseTable t) Identity
  -> View t
  -> m [D n t Identity]
runSetViewAround = undefined

