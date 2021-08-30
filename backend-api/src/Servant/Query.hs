{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module: Servant.Query
Description: Types for specifying common query parameters
-}
module Servant.Query
  ( -- * Generic crud routes
    CrudRoutes(..)
  , defaultCrud
  ) where

import           Prelude

import           Control.Monad                  ( unless )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text.Lazy                as LText
                                                ( fromStrict )
import           Database.Beam
import           Database.Beam.API
import           Database.Beam.Backend.SQL      ( BeamSqlBackendCanSerialize )
import           Database.Beam.Backend.SQL.BeamExtensions
                                                ( MonadBeamInsertReturning )
import           Database.Beam.Expand           ( ToName )
import           Database.Beam.Extra
import           Database.Beam.Operator
import           Database.Beam.Postgres         ( Postgres )
import           Database.Beam.Schema.Tables
import           GHC.Generics                   ( Generic )
import           Servant.API
import           Servant.API.Generic            ( (:-) )
import           Servant.Crud.Server.API
import           Servant.Crud.Server.Deriving   ( typeName )
import           Servant.Crud.Server.Headers
import           Servant.Crud.Server.QueryObject
                                                ( ToQueryText )
import           Servant.Crud.Server.QueryOperator
                                                ( Filter )
import           Servant.Server                 ( ServerError
                                                , err404
                                                )
import           Servant.Server.Generic         ( AsServerT )

-- |  Removes a Maybe at the cost of a 404 error with an automatic error message
notFound
  :: forall m t
   . (MonadError ServerError m, Typeable t)
  => Maybe (t Identity)
  -> m (t Identity)
notFound = justOr404
  (  "Row not found in table "
  <> LText.fromStrict (typeName (Proxy :: Proxy (t Identity)))
  )

-- |  Removes a Maybe at the cost of a 404 error with an automatic error message
insertErr
  :: forall m t
   . (MonadError ServerError m, Typeable t)
  => Maybe (PrimaryKey t Identity)
  -> m (PrimaryKey t Identity)
insertErr = justOr500
  (  "Error inserting row in table "
  <> LText.fromStrict (typeName (Proxy :: Proxy (t Identity)))
  )

throwNotFound :: (MonadError ServerError m) => m a
throwNotFound = throwError err404

-- | A common set of CRUD routes
-- Use this to define an entire CRUD endpoint at once
--
-- > type UserAPI = "users" :> ToServantApi (CrudRoutes UserT)
--
-- This will create GET, POST for the endpoint /users
-- and GET, PUT, PATCH, DELETE for the endpoint /users/:id.
-- GET /users accepts filters as described by 'Filter',
-- ordering as described by 'OrderBy'
-- and "offset" and "limit" parameters.
data CrudRoutes t t2 route = CrudRoutes
  { _get           :: route :- Get_ t
  , _put           :: route :- Put_ t2
  , _patch         :: route :- Patch_ t2
  , _delete        :: route :- Delete_ t2
  , _post          :: route :- Post_ t2
  , _getList       :: route :- GetList Postgres t
  , _getListLabels :: route :- GetListLabels Postgres t
  , _deleteList    :: route :- DeleteList_ Postgres t
  , _postList      :: route :- PostList t2
  }
  deriving Generic


defaultCrud
  :: ( Table t
     , ToName t
     , Table t2
     , ToHttpApiData (PrimaryKey t2 Identity)
     , FromBackendRow Postgres (t Identity)
     , FromBackendRow Postgres (t2 Identity)
     , FromBackendRow Postgres (PrimaryKey t Identity)
     , FieldsFulfillConstraintNullable (BeamSqlBackendCanSerialize Postgres) t2
     , FieldsFulfillConstraintFilter (Operator Filter Postgres) t
     , FieldsFulfillConstraint (HasSqlEqualityCheck Postgres) (PrimaryKey t)
     , FieldsFulfillConstraint (HasSqlEqualityCheck Postgres) (PrimaryKey t2)
     , MonadBeamInsertReturning Postgres m
     , MonadError ServerError m1
     , SqlValableTable Postgres (PrimaryKey t2)
     , SqlValableTable Postgres (PrimaryKey t)
     , SqlValableTable Postgres t2
     , ToQueryText (t Filter)
     )
  => (forall b . m b -> m1 b)  -- ^ Transform to the app monad
  -> DatabaseEntity Postgres db (TableEntity t2) -- ^ The database entity
  -> (PrimaryKey t2 Identity -> PrimaryKey t Identity) -- ^ Function to change the type of the primary key
  -> (PrimaryKey t Identity -> PrimaryKey t2 Identity) -- ^ Function to change the type of the primary key back
  -> (forall s . Q Postgres db s (t (QExpr Postgres s))) -- ^ The collection
  -> CrudRoutes t t2 (AsServerT m1)
defaultCrud runDB db coerce coerceBack coll = CrudRoutes
  { _get           = \key -> runDB (runLookupIn key coll) >>= notFound
  , _put           = \key -> ifInView_ key . runSaveWithId db key
  , _patch         = \key -> ifInView_ key . runPatch db key
  , _delete        = \key -> ifInView_ key $ runDeleteKey db key
  , _post          = \path x ->
                       hLocation' (removeZero path)
                         <$> (runDB (runInsertOne db x) >>= insertErr)
  , _getList       = \path view ->
                       hTotalLink' path view
                         <$> runDB (runCountInView view coll)
                         <*> runDB (runSetView view coll)
  , _getListLabels = \path view ->
                       hTotalLink' path view
                         <$> runDB (runCountInView view coll)
                         <*> runDB (runNamesInView view coll)
  , _deleteList    = \keys fltr -> do
                       keys' <- runDB (runPrimaryKeysInView fltr keys coll)
                       runDB (runDeleteKeys db (fmap coerceBack keys'))
                       pure keys'
  , _postList      = runDB . runInsertMany db
  }
 where
  noc = fmap (const NoContent)
  ifInView_ key = noc . ifInView key
  ifInView key fn = do
    found <- runDB $ runIsInView (coerce key) coll
    unless found throwNotFound
    runDB fn
  removeZero (PathInfo xs) = PathInfo (init xs)
