{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
  )
where

import           Prelude                 hiding ( div )

import           Data.Default
import           Data.Text                      ( Text )
import           Database.Beam
import           Database.Beam.API
import           Database.Beam.Postgres         ( Postgres )
import           Servant
import           Servant.Crud.Server.API
import           Servant.Crud.Server.QueryOperator
                                         hiding ( ParamKind(..) )
import           Servant.Docs                   ( DocQueryParam(..)
                                                , ParamKind(..)
                                                , ToParam(..)
                                                , ToSample(..)
                                                )
import           Servant.Server.Generic         ( AsServerT )

import           Servant.Query

import           Auth
import           Common.Api
import           Context
import           Schema

type Api = Api' Postgres
type BlogApi = BlogApi' Postgres

-- | The blog server
blogServer :: AppServer BlogApi
blogServer =
  const (_get gBlog)
    :<|> const (_put gBlog)
    :<|> const (_patch gBlog)
    :<|> const (_delete gBlog)
    :<|> const (_deleteList gBlog)
    :<|> const (_post gBlog)
    :<|> const (_postList gBlog)
    :<|> getBlogs
 where
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
