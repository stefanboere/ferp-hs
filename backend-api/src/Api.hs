{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Api
Description: Specifies the combined api
-}
module Api
  ( module Common.Api
  , server
  , api
  , Api
  ) where

import           Prelude                 hiding ( div )

import           Control.Concurrent.STM         ( atomically )
import           Control.Monad.Reader           ( asks )
import           Data.Default
import           Data.Text                      ( Text )
import           Database.Beam
import           Database.Beam.API
import           Database.Beam.Postgres         ( Postgres )
import           Servant
import           Servant.Crud.Server.API
import           Servant.Crud.Server.Headers    ( PathInfo(..) )
import           Servant.Crud.Server.QueryObject
                                                ( QObj )
import           Servant.Crud.Server.QueryOperator
                                         hiding ( ParamKind(..) )
import           Servant.Docs                   ( DocQueryParam(..)
                                                , ParamKind(..)
                                                , ToParam(..)
                                                , ToSample(..)
                                                )
import           Servant.Server.Generic         ( AsServerT )
import           Servant.Subscriber             ( Event(..)
                                                , Subscriber
                                                , notify
                                                )
import           Servant.Subscriber.Subscribable

import           Servant.Query

import           Auth
import           Common.Api
import           Context
import           Schema

type Api = Api' Postgres
type BlogApi = BlogApi' Postgres

type instance IsElem' sa (Auth r :> sb) = IsElem sa sb
type instance IsElem' sa (PathInfo :> sb) = IsElem sa sb
type instance IsElem' sa (QObj a :> sb) = IsElem sa sb

type instance IsSubscribable' sa (Auth r :> sb) = IsSubscribable sa sb
type instance IsSubscribable' sa (PathInfo :> sb) = IsSubscribable sa sb
type instance IsSubscribable' sa (QObj r :> sb) = IsSubscribable sa sb

-- For us any GET endpoint is subscribable
type instance IsSubscribable' sa (Get '[JSON , CSV] r)
  = ()

notifyBlogs :: MonadIO m => Subscriber (Api' Postgres) -> m ()
notifyBlogs sub = liftIO $ atomically $ notify
  sub
  ModifyEvent
  (Proxy :: Proxy ("blogs" :> Get '[JSON, CSV] (GetListHeaders Blog)))
  linkURI

notifyBlog :: MonadIO m => Subscriber (Api' Postgres) -> BlogId -> m ()
notifyBlog sub blogid = liftIO $ atomically $ notify
  sub
  ModifyEvent
  (Proxy :: Proxy ("blogs" :> CaptureId BlogT :> Get '[JSON] Blog))
  (linkURI . ($ blogid))

-- | The blog server
blogServer :: AppServer BlogApi
blogServer =
  _get gBlog
    :<|> const (_put gBlog)
    :<|> const patchBlogs
    :<|> const (_delete gBlog)
    :<|> const (_deleteList gBlog)
    :<|> const (_post gBlog)
    :<|> const (_postList gBlog)
    :<|> getBlogs
 where
  patchBlogs i b = do
    x   <- _patch gBlog i b
    sub <- asks getSubscriber
    notifyBlogs sub
    notifyBlog sub i
    pure x

  getBlogs :: AppServer (GetList Postgres BlogT)
  getBlogs pinfo v = _getList gBlog pinfo newView
   where
    newView = v { filters = newFilt }
  --  newOrd  = ord { blogDate = Ordering Desc (-1) } -- Force the blogs to be ordered newest first

    filt    = filters v

    maximumMaybe [] = Nothing
    maximumMaybe xs = Just $ maximum xs

    newFilt = case maximumMaybe [] of
      Just Administrator -> filt
      Just Extra ->
        filt { _blogIsPublished = setf @"" [True] $ _blogIsPublished filt }
      Just Regular -> filt
        { _blogIsExtra     = setf @"" [False] $ _blogIsExtra filt
        , _blogIsPublished = setf @"" [True] $ _blogIsPublished filt
        }
      Nothing -> filt { _blogIsPublished = def }

  gBlog :: CrudRoutes BlogT BlogT (AsServerT App)
  gBlog = defaultCrud runDB (_appDatabaseBlogs appDatabase) id
    $ all_ (_appDatabaseBlogs appDatabase)

instance ToParam (QueryParam "key" Text) where
  toParam _ = DocQueryParam "key" [] "An authorization key" Normal

instance ToSample Text where
  toSamples _ = []

-- | A proxy of the api
api :: Proxy Api
api = Proxy

-- | The combined server
server :: AppServer Api
server = blogServer
