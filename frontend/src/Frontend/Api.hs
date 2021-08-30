{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend.Api
  ( getBlog
  , putBlog
  , patchBlog
  , deleteBlog
  , deleteBlogs
  , postBlog
  , postBlogs
  , getBlogs
  , getBlogsApiLink
  -- * Utils
  , getListToMapsubset
  , runApi
  , withXsrfHeader
  , usingCookie
  , refreshAccessTokenEvery
  , ApiWidget
  ) where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Time                      ( NominalDiffTime )
import           Language.Javascript.JSaddle    ( MonadJSM )
import           Reflex.Dom              hiding ( Client
                                                , Link(..)
                                                , rangeInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.AccessControl          ( Token(..) )
import           Servant.Crud.API
import           Servant.Crud.Headers           ( ExceptLimited(..)
                                                , Offset(..)
                                                , TotalCount(..)
                                                )
import           Servant.Crud.QueryOperator     ( Filter )
import qualified Servant.Subscriber.Reflex     as Sub
import           Servant.Subscriber.Reflex      ( ApiWidget
                                                , FreeClient
                                                )

import           Common.Api
import           Common.Schema
import           Components.Table               ( MapSubset(..) )

getListToMapsubset :: GetListHeaders a -> MapSubset Int a
getListToMapsubset resp = MapSubset
  (Map.fromList $ zip [x0 ..] (getResponse resp))
  (getCount resp)
 where
  x0 = fromIntegral $ fromMaybe 0 (getOffset resp)
  getCount x =
    case lookupResponseHeader x :: ResponseHeader "X-Total-Count" TotalCount of
      Servant.API.Header (TotalCount c) -> Just c
      _ -> Nothing

  getOffset x =
    case lookupResponseHeader x :: ResponseHeader "X-Offset" Offset of
      Servant.API.Header (Offset c) -> Just c
      _                             -> Nothing

-- | Supplying a token is not needed for xhr because the cookie is sent.
-- This is a utility for this.
usingCookie :: Token
usingCookie = Token ""

withXsrfHeader :: MonadJSM m => XhrRequest a -> m (XhrRequest a)
withXsrfHeader r@(XhrRequest _ _ cfg) = do
  cookie <- Sub.findCookie "XSRF-TOKEN"
  let addXsrf = maybe id (Map.insert "X-XSRF-TOKEN") cookie
  let c' = cfg
        { _xhrRequestConfig_headers = addXsrf (_xhrRequestConfig_headers cfg)
        , _xhrRequestConfig_withCredentials = True
        }
  pure $ r { _xhrRequest_config = c' }

refreshAccessTokenEvery
  :: (Applicative m, Prerender js t m) => NominalDiffTime -> m ()
refreshAccessTokenEvery interval = prerender_ (pure ()) $ do
  tickEv <- tickLossyFromPostBuildTime interval
  refreshAccessTokenXhr (() <$ tickEv)

refreshAccessTokenXhr
  :: ( MonadIO m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , HasJSContext (Performable m)
     , TriggerEvent t m
     )
  => Event t ()
  -> m ()
refreshAccessTokenXhr ev = ignore <$> getAndDecode ("/auth/refresh" <$ ev)
 where
  ignore :: Event t (Maybe ()) -> ()
  ignore _ = ()

runApi
  :: (MonadHold t m, MonadFix m, Prerender js t m) => ApiWidget t m a -> m a
runApi = Sub.runApiWidget "ws://localhost:3005/subscriber"

getBlog :: BlogId -> FreeClient Blog
putBlog :: Token -> BlogId -> Blog -> FreeClient NoContent
patchBlog :: Token -> BlogId -> BlogPatch -> FreeClient NoContent
deleteBlog :: Token -> BlogId -> FreeClient NoContent
deleteBlogs
  :: Token -> ExceptLimited [BlogId] -> BlogT Filter -> FreeClient [BlogId]
postBlog :: Token -> Blog -> FreeClient (Headers '[LocationHdr] BlogId)
postBlogs :: Token -> [Blog] -> FreeClient [BlogId]
getBlogs :: View Be BlogT -> FreeClient (GetListHeaders Blog)
getBlog :<|> putBlog :<|> patchBlog :<|> deleteBlog :<|> deleteBlogs :<|> postBlog :<|> postBlogs :<|> getBlogs
  = Sub.client clientApi

getBlogsApiLink :: View Be BlogT -> Servant.API.Link
getBlogsApiLink =
  safeLink clientApi (Proxy :: Proxy ("blogs" :> GetList Be BlogT))
