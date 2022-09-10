{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module: Servant.Query
Description: Types for specifying common query parameters
-}
module Servant.Query
  ( -- * Generic crud routes
    CrudRoutes(..)
  , defaultCrud
  -- * Default route implementation
  , defaultGet
  , defaultPut
  , defaultPatch
  , defaultDelete
  , defaultPost
  , defaultGetList
  , defaultGetListLabels
  , defaultDeleteList
  , defaultPostList
  -- * Advanced default routes
  , defaultGetWith
  , defaultGetListWith
  , teapot
  ) where

import           Prelude

import           Control.Monad                  ( unless )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Data.Functor.Identity          ( Identity )
import           Data.Monoid                    ( Last )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text.Lazy                as LText
                                                ( fromStrict )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           ProjectM36.Atomable            ( Atomable )
import           ProjectM36.Beamable
import           ProjectM36.Client.Monad
import           Servant.API             hiding ( Link )
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
                                                , err418
                                                )
import           Servant.Server.Generic         ( AsServerT )

-- |  Removes a Maybe at the cost of a 404 error with an automatic error message
notFound
  :: forall m (t :: (* -> *) -> *) a
   . (MonadError ServerError m, Typeable t)
  => Proxy t
  -> Maybe a
  -> m a
notFound _ = justOr404
  (  "Row not found in table "
  <> LText.fromStrict (typeName (Proxy :: Proxy (t Identity)))
  )

throwNotFound :: (MonadError ServerError m) => m a
throwNotFound = throwError err404


ifInView
  :: ( IsView PrimaryKey t
     , Table (BaseTable t)
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , MonadM36 mp
     , MonadError ServerError ms
     )
  => (forall b . mp b -> ms b)
  -> Proxy t
  -> PrimaryKey (BaseTable t) Identity
  -> ms a
  -> ms a
ifInView runDB pt key fn = do
  found <- runDB $ runIsInView pt key
  unless found throwNotFound
  fn

defaultGetWith
  :: ( IsView n t
     , Table (BaseTable t)
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , MonadM36 mp
     , MonadError ServerError ms
     , Typeable t
     )
  => (forall a . mp a -> ms a)
  -> Proxy n
  -> Proxy t
  -> PrimaryKey (BaseTable t) Identity
  -> ms (D n t Identity)
defaultGetWith runDB pn pt key = runDB (runLookupIn pn pt key) >>= notFound pt

defaultGet
  :: forall t mp ms
   . ( IsView Full t
     , Table (BaseTable t)
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , MonadM36 mp
     , MonadError ServerError ms
     , Typeable t
     )
  => (forall a . mp a -> ms a)
  -> PrimaryKey (BaseTable t) Identity
  -> ms (t Identity)
defaultGet runDB = defaultGetWith runDB pn pt
 where
  pn = Proxy :: Proxy Full
  pt = Proxy :: Proxy t

defaultPut
  :: ( Table (BaseTable t)
     , FieldsFulfillConstraint Atomable (BaseTable t)
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , IsView PrimaryKey t
     , MonadError ServerError ms
     , MonadM36 mp
     )
  => (forall a . mp a -> ms a)
  -> Proxy t
  -> PrimaryKey (BaseTable t) Identity
  -> BaseTable t Identity
  -> ms NoContent
defaultPut runDB pt key dat =
  NoContent <$ ifInView runDB pt key (runDB $ runSave key dat)

defaultPatch
  :: ( Table (BaseTable t)
     , FieldsFulfillConstraintNullable Atomable (BaseTable t)
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , IsView PrimaryKey t
     , MonadError ServerError ms
     , MonadM36 mp
     )
  => (forall a . mp a -> ms a)
  -> Proxy t
  -> PrimaryKey (BaseTable t) Identity
  -> BaseTable t Last
  -> ms NoContent
defaultPatch runDB pt key dat =
  NoContent <$ ifInView runDB pt key (runDB $ runPatch key dat)

defaultDelete
  :: ( Table (BaseTable t)
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , IsView PrimaryKey t
     , MonadError ServerError ms
     , MonadM36 mp
     )
  => (forall a . mp a -> ms a)
  -> Proxy t
  -> PrimaryKey (BaseTable t) Identity
  -> ms NoContent
defaultDelete runDB pt key =
  NoContent <$ ifInView runDB pt key (runDB $ runDeleteKey key)

-- TODO remove the t ~ BaseTable t restriction
defaultDeleteList
  :: ( Table (BaseTable t)
     , FieldsFulfillConstraintFilter (Operator Filter) t
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , MonadM36 mp
     , IsView PrimaryKey t
     , t ~ BaseTable t
     )
  => (forall a . mp a -> ms a)
  -> ExceptLimited [PrimaryKey (BaseTable t) Identity]
  -> t Filter
  -> ms [PrimaryKey (BaseTable t) Identity]
defaultDeleteList runDB keys fltr = runDB $ do
  keys' <- runPrimaryKeysInView fltr keys
  runDeleteKeys keys'
  pure keys'

defaultPost
  :: ( Table t
     , FieldsFulfillConstraint Atomable t
     , Monad ms
     , MonadM36 mp
     , ToHttpApiData (PrimaryKey t Identity)
     )
  => (forall a . mp a -> ms a)
  -> PathInfo
  -> t Identity
  -> ms
       ( Headers
           '[Header "Location" PathInfo]
           (PrimaryKey t Identity)
       )
defaultPost runDB path dat = do
  runDB $ runInsertOne dat
  let pk = primaryKey dat
  pure $ hLocation' (removeZero path) pk
  where removeZero (PathInfo xs) = PathInfo (init xs)

defaultPostList
  :: (Table t, FieldsFulfillConstraint Atomable t, Monad ms, MonadM36 mp)
  => (forall a . mp a -> ms a)
  -> [t Identity]
  -> ms [PrimaryKey t Identity]
defaultPostList runDB xs =
  runDB (runInsertMany xs) >> pure (fmap primaryKey xs)

-- TODO remove the t ~ BaseTable t restriction
defaultGetListWith
  :: ( IsView n t
     , Table (BaseTable t)
     , MonadM36 mp
     , Monad ms
     , ToQueryText (t Filter)
     , FieldsFulfillConstraintFilter (Operator Filter) t
     , t ~ BaseTable t
     )
  => (forall a . mp a -> ms a)
  -> Proxy n
  -> Proxy t
  -> PathInfo
  -> View t
  -> ms
       ( Headers
           '[ Header "X-Offset" Offset
            , Header "X-Total-Count" TotalCount
            , Header "Link" (Link URI)
            , Header "Link" (Link URI)
            ]
           [D n t Identity]
       )
defaultGetListWith runDB pn pt path view@(View _ _ filt) = do
  c  <- runDB $ runCountIn pn pt (matching_ filt)
  xs <- case c of
    Just 0 -> pure [] -- No need to lookup the results again
    _      -> runDB $ runSetView pn view
  pure $ hTotalLink' path view (TotalCount <$> c) xs

-- TODO remove the t ~ BaseTable t restriction
defaultGetList
  :: forall t mp ms
   . ( IsView Full t
     , Table (BaseTable t)
     , MonadM36 mp
     , Monad ms
     , ToQueryText (t Filter)
     , FieldsFulfillConstraintFilter (Operator Filter) t
     , t ~ BaseTable t
     )
  => (forall a . mp a -> ms a)
  -> PathInfo
  -> View t
  -> ms
       ( Headers
           '[ Header "X-Offset" Offset
            , Header "X-Total-Count" TotalCount
            , Header "Link" (Link URI)
            , Header "Link" (Link URI)
            ]
           [t Identity]
       )
defaultGetList runDB = defaultGetListWith runDB pn pt
 where
  pn = Proxy :: Proxy Full
  pt = Proxy :: Proxy t

-- TODO remove the t ~ BaseTable t restriction
defaultGetListLabels
  :: forall t mp ms
   . ( IsView Named t
     , Table (BaseTable t)
     , MonadM36 mp
     , Monad ms
     , ToQueryText (t Filter)
     , FieldsFulfillConstraintFilter (Operator Filter) t
     , t ~ BaseTable t
     )
  => (forall a . mp a -> ms a)
  -> PathInfo
  -> View t
  -> Maybe (PrimaryKey (BaseTable t) Identity)
  -> ms
       ( Headers
           '[ Header "X-Offset" Offset
            , Header "X-Total-Count" TotalCount
            , Header "Link" (Link URI)
            , Header "Link" (Link URI)
            ]
           [Named t Identity]
       )
defaultGetListLabels runDB path v _ = defaultGetListWith runDB pn pt path v
 where
  pn = Proxy :: Proxy Named
  pt = Proxy :: Proxy t

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
  , _getList       :: route :- GetList t
  , _getListLabels :: route :- GetListLabels t
  , _deleteList    :: route :- DeleteList_ t
  , _postList      :: route :- PostList t2
  }
  deriving Generic


defaultCrud
  :: ( FieldsFulfillConstraint Atomable (BaseTable t)
     , FieldsFulfillConstraint Atomable (PrimaryKey (BaseTable t))
     , FieldsFulfillConstraintFilter (Operator Filter) t
     , FieldsFulfillConstraintNullable Atomable (BaseTable t)
     , IsView Full t
     , IsView PrimaryKey t
     , MonadError ServerError ms
     , MonadM36 mp
     , Table (BaseTable t)
     , ToHttpApiData (PrimaryKey (BaseTable t) Identity)
     , ToName t
     , ToQueryText (t Filter)
     , t ~ BaseTable t
     ) -- TODO remove this constraint
  => (forall a . mp a -> ms a)
  -> Proxy t
  -> CrudRoutes t (BaseTable t) (AsServerT ms)
defaultCrud runDB pt = CrudRoutes { _get           = defaultGet runDB
                                  , _put           = defaultPut runDB pt
                                  , _patch         = defaultPatch runDB pt
                                  , _delete        = defaultDelete runDB pt
                                  , _post          = defaultPost runDB
                                  , _getList       = defaultGetList runDB
                                  , _getListLabels = defaultGetListLabels runDB
                                  , _deleteList    = defaultDeleteList runDB
                                  , _postList      = defaultPostList runDB
                                  }

-- | Implement the routs by always returning 418 - I'm a teapot
teapot :: (MonadError ServerError m) => CrudRoutes t t2 (AsServerT m)
teapot = CrudRoutes { _get           = const throwTeapot
                    , _put           = const (const throwTeapot)
                    , _patch         = const (const throwTeapot)
                    , _delete        = const throwTeapot
                    , _post          = const (const throwTeapot)
                    , _getList       = const (const throwTeapot)
                    , _getListLabels = const (const (const throwTeapot))
                    , _deleteList    = const (const throwTeapot)
                    , _postList      = const throwTeapot
                    }
  where throwTeapot = throwError err418
