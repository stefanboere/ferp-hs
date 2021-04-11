{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Api
Description: Specifies the combined api
-}
module Api
  ( Api
  , api
  , server
  )
where

import           Prelude                 hiding ( div )

import           Data.Text                      ( Text )

import           Data.Default
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
import           Context
import           Schema

{-# ANN module ("HLint: ignore Redundant bracket" :: String) #-}
-- | The api
type Api = ("blogs" :> BlogApi)

-- BLOGS

-- | A slightly different api than the generic crud api. This is because we
-- need to handle the permissions differently.
-- It really is a different api therefore, but we can still reuse our existing generic implementation
-- brittany-disable-next-binding
type BlogApi
  = (Auth Admin :> Get_ BlogT)
      :<|>
      (Auth Admin :> Put_ BlogT)
      :<|>
      (Auth Admin :> Patch_ BlogT)
      :<|>
      (Auth Admin :> Delete_ BlogT)
      :<|>
      (Auth Admin :> DeleteList_ BlogT)
      :<|>
      (Auth Admin :> Post_ BlogT)
      :<|>
      (Auth Admin :> PostList BlogT)
      :<|>
      (GetList Postgres BlogT)



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
        filt { blogIsPublished = setf @"" [True] $ blogIsPublished filt }
      Just Regular -> filt
        { blogIsExtra     = setf @"" [False] $ blogIsExtra filt
        , blogIsPublished = setf @"" [True] $ blogIsPublished filt
        }
      Nothing -> filt { blogIsPublished = def }

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
