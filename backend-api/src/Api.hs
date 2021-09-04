{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Control.Monad.Reader           ( MonadReader(..)
                                                , asks
                                                )
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
type ChannelApi = ChannelApi' Postgres

type instance IsElem' sa (Auth r :> sb) = IsElem sa sb
type instance IsElem' sa (PathInfo :> sb) = IsElem sa sb
type instance IsElem' sa (QObj a :> sb) = IsElem sa sb

type instance IsSubscribable' sa (Auth r :> sb) = IsSubscribable sa sb
type instance IsSubscribable' sa (PathInfo :> sb) = IsSubscribable sa sb
type instance IsSubscribable' sa (QObj r :> sb) = IsSubscribable sa sb

-- For us any GET endpoint is subscribable
type instance IsSubscribable' sa (Get '[JSON , CSV] r)
  = ()

instance ToParam (QueryParam "key" Text) where
  toParam _ = DocQueryParam "key" [] "An authorization key" Normal

instance ToParam (QueryParam "around" (PrimaryKey t Identity)) where
  toParam _ = DocQueryParam "around"
                            []
                            "Jump to the page in which this primary key lies"
                            Normal

instance ToSample Text where
  toSamples _ = []

-- | A proxy of the api
api :: Proxy Api
api = Proxy

-- | The combined server
server :: AppServer Api
server = blogServer :<|> channelServer

type GetListSimple t = Get '[JSON , CSV] (GetListHeaders (t Identity))
type GetSimple t = CaptureId t :> Get '[JSON] (t Identity)

notifyModified
  :: ( MonadReader AppConfig m
     , MonadIO m
     , IsElem endpoint (Api' Postgres)
     , HasLink endpoint
     , IsValidEndpoint endpoint
     , IsSubscribable endpoint (Api' Postgres)
     )
  => Proxy endpoint
  -> (MkLink endpoint Servant.Link -> URI)
  -> m ()
notifyModified p mkLnk = do
  sub <- asks getSubscriber
  liftIO $ atomically $ notify sub ModifyEvent p mkLnk

notifyBlogs :: (MonadReader AppConfig m, MonadIO m) => m ()
notifyBlogs =
  notifyModified (Proxy :: Proxy ("blogs" :> GetListSimple BlogT)) linkURI

notifyBlog :: (MonadReader AppConfig m, MonadIO m) => BlogId -> m ()
notifyBlog blogid = notifyModified
  (Proxy :: Proxy ("blogs" :> GetSimple BlogT))
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
    :<|> getBlogsLabels
 where
  patchBlogs i b = do
    x <- _patch gBlog i b
    notifyBlogs
    notifyBlog i
    pure x

  getBlogs :: AppServer (GetList Postgres BlogT)
  getBlogs pinfo v = _getList gBlog pinfo (buildNewView v)

  getBlogsLabels :: AppServer (GetListLabels Postgres BlogT)
  getBlogsLabels pinfo v = _getListLabels gBlog pinfo (buildNewView v)

  buildNewView v = v { filters = newFilt }
   where
  --  newOrd  = ord { blogDate = Ordering Desc (-1) } -- Force the blogs to be ordered newest first

    filt = filters v

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
  gBlog = defaultCrud runDB (_appDatabaseBlogs appDatabase) id id
    $ all_ (_appDatabaseBlogs appDatabase)


notifyChannels :: (MonadReader AppConfig m, MonadIO m) => m ()
notifyChannels =
  notifyModified (Proxy :: Proxy ("channels" :> GetListSimple ChannelT)) linkURI

notifyChannel :: (MonadReader AppConfig m, MonadIO m) => ChannelId -> m ()
notifyChannel pk' = notifyModified
  (Proxy :: Proxy ("channels" :> GetSimple ChannelT))
  (linkURI . ($ pk'))

-- | The blog server
channelServer :: AppServer ChannelApi
channelServer =
  _get genericImpl
    :<|> const (_put genericImpl)
    :<|> const patchServer
    :<|> const (_delete genericImpl)
    :<|> const (_deleteList genericImpl)
    :<|> const (_post genericImpl)
    :<|> const (_postList genericImpl)
    :<|> _getList genericImpl
    :<|> _getListLabels genericImpl
 where
  patchServer i b = do
    x <- _patch genericImpl i b
    notifyChannels
    notifyChannel i
    pure x

  genericImpl :: CrudRoutes ChannelT ChannelT (AsServerT App)
  genericImpl = defaultCrud runDB (_appDatabaseChannels appDatabase) id id
    $ all_ (_appDatabaseChannels appDatabase)

