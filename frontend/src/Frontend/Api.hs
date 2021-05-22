{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  -- * Free clients
  , getBlogF
  , putBlogF
  , patchBlogF
  , deleteBlogF
  , deleteBlogsF
  , postBlogF
  , postBlogsF
  , getBlogsF
  -- * Utils
  , orAlert
  , requestBtn
  , orAlert'
  , orAlertBg
  )
where

import           Control.Monad                  ( (>=>) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Map                      as Map
import           Data.Proxy
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.TypeLits                   ( KnownSymbol
                                                , symbolVal
                                                )
import           GHCJS.DOM                     as DOM
import           GHCJS.DOM.Document            as DOM
import           GHCJS.DOM.Window              as DOM
import           Language.Javascript.JSaddle    ( MonadJSM )
import           Reflex.Dom              hiding ( Client
                                                , Link(..)
                                                , rangeInput
                                                )
import qualified Reflex.Dom.Prerender          as Prerender
                                                ( Client )
import           Servant.API             hiding ( URI(..) )
import           Servant.AccessControl          ( Auth'
                                                , Token
                                                )
import           Servant.Common.Req             ( fanReqResult' )
import           Servant.Crud.API
import           Servant.Crud.QueryObject       ( QueryObject
                                                , ToQueryText(..)
                                                , toQueryText
                                                )
import           Servant.Reflex
import qualified Servant.Subscriber.Reflex     as Sub
import           Servant.Subscriber.Reflex      ( FreeClient )

import           Common.Api
import           Common.Schema
import           Components.Alert
import           Components.Button
import           Components.Class

instance (Reflex t, HasClient t m api tag) => HasClient t m (Auth' auths a v :> api) tag where
  type Client t m (Auth' auths a v :> api) tag = Client t m api tag
  clientWithRouteAndResultHandler Proxy q t req burl opts wrp =
    clientWithRouteAndResultHandler (Proxy :: Proxy api) q t req burl opts wrp

instance (HasClient t m api tag) => HasClient t m (PathInfo :> api) tag where
  type Client t m (PathInfo :> api) tag = Client t m api tag
  clientWithRouteAndResultHandler Proxy q t req burl opts wrp =
    clientWithRouteAndResultHandler (Proxy :: Proxy api) q t req burl opts wrp

instance (Reflex t, HasClient t m api tag, ToQueryText a, KnownSymbol sym)
    => HasClient t m (QueryObject sym a :> api) tag where
  type Client t m (QueryObject sym a :> api) tag
    = Dynamic t (Either Text a) -> Client t m api tag

  clientWithRouteAndResultHandler Proxy q t req burl opts wrp param =
    clientWithRouteAndResultHandler (Proxy :: Proxy api) q t req' burl opts wrp
   where
    req'      = req { qParams = ps : qParams req }

    ps        = ("", QueryPartObject $ fmap (toQueryText paramname) <$> param)

    paramname = Text.pack $ symbolVal (Proxy :: Proxy sym)

-- * BLOGS
getBlog
  :: SupportsServantReflex t m
  => Dynamic t (Either Text BlogId)
  -> Event t tag
  -> m (Event t (ReqResult tag Blog))
putBlog
  :: SupportsServantReflex t m
  => Dynamic t (Either Text BlogId)
  -> Dynamic t (Either Text Blog)
  -> Event t tag
  -> m (Event t (ReqResult tag NoContent))
patchBlog
  :: SupportsServantReflex t m
  => Dynamic t (Either Text BlogId)
  -> Dynamic t (Either Text BlogPatch)
  -> Event t tag
  -> m (Event t (ReqResult tag NoContent))
deleteBlog
  :: SupportsServantReflex t m
  => Dynamic t (Either Text BlogId)
  -> Event t tag
  -> m (Event t (ReqResult tag NoContent))
deleteBlogs
  :: SupportsServantReflex t m
  => Dynamic t (Either Text [BlogId])
  -> Event t tag
  -> m (Event t (ReqResult tag NoContent))
postBlog
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Blog)
  -> Event t tag
  -> m (Event t (ReqResult tag (Headers '[LocationHdr] NoContent)))
postBlogs
  :: SupportsServantReflex t m
  => Dynamic t (Either Text [Blog])
  -> Event t tag
  -> m (Event t (ReqResult tag [BlogId]))
getBlogs
  :: SupportsServantReflex t m
  => Dynamic t (Either Text (View Be BlogT))
  -> Event t tag
  -> m (Event t (ReqResult tag (GetListHeaders Blog)))
getBlog :<|> putBlog :<|> patchBlog :<|> deleteBlog :<|> deleteBlogs :<|> postBlog :<|> postBlogs :<|> getBlogs
  = clientWithOpts clientApi Proxy Proxy (constDyn url)
    $ ClientOptions (withCredentials (const (pure True)) >=> withXsrfHeader)
  where url = BaseFullUrl Http "localhost" 3005 ""

orAlertBg
  :: ( Prerender js t m
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Prerender.Client m (Event t (ReqResult tag a))
  -> m (Event t a)
orAlertBg reqEv = do
  resultEv <- prerender (pure never) reqEv
  fmap snd <$> orAlert' (switchDyn resultEv)

orAlert
  :: ( Prerender js t m
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t (ReqResult tag a)
  -> m (Event t a)
orAlert = fmap (fmap snd) . orAlert'

orAlert'
  :: ( Prerender js t m
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t (ReqResult tag a)
  -> m (Event t (tag, a))
orAlert' resultEv = do
  let (errEv, rEv) = fanReqResult' resultEv
  alerts def { _alertConfig_status = Danger } (snd <$> errEv)
  pure rEv

requestBtn
  :: ( DomBuilder t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => (Dynamic t ActionState -> m (Event t ()))
  -> (Event t () -> Prerender.Client m (Event t (ReqResult tag a)))
  -> Dynamic t Bool
  -> Event t ()
  -> m (Event t (ReqResult tag a))
requestBtn mkBtn req dynDisabled reqEvAuto = do

  rec dynState <- holdDyn def $ leftmost
        [ ActionLoading <$ reqEv
        , responseState <$> resultEv
        , def <$ resultEvDelay
        ]

      reqEvMan <- mkBtn ((disabledState <$> dynDisabled) <> dynState)
      let reqEv = leftmost [reqEvAuto, reqEvMan]

      resultEvDyn <- prerender (pure never) (req reqEv)
      let resultEv = switchDyn resultEvDyn

      resultEvDelay <- debounce 2 resultEv

  pure resultEv
 where
  disabledState True  = ActionDisabled
  disabledState False = ActionAvailable

  responseState ResponseSuccess{} = ActionSuccess
  responseState _                 = ActionError


withXsrfHeader :: MonadJSM m => XhrRequest a -> m (XhrRequest a)
withXsrfHeader r@(XhrRequest _ _ cfg) = do
  cookie <- findCookie "XSRF-TOKEN"
  let addXsrf = maybe id (Map.insert "X-XSRF-TOKEN") cookie
  let c' = cfg
        { _xhrRequestConfig_headers = addXsrf (_xhrRequestConfig_headers cfg)
        }
  pure $ r { _xhrRequest_config = c' }

findCookie :: MonadJSM m => Text -> m (Maybe Text)
findCookie x = do
  window  <- DOM.currentWindowUnchecked
  doc     <- DOM.getDocument window
  cookies <- DOM.getCookie doc
  let cs =
        fmap (fmap (Text.dropWhile (== '=')) . Text.span (/= '=') . Text.strip)
          . Text.splitOn ";"
          $ cookies

  pure (lookup x cs)

getBlogF :: Token -> BlogId -> FreeClient Blog
putBlogF :: Token -> BlogId -> Blog -> FreeClient NoContent
patchBlogF :: Token -> BlogId -> BlogPatch -> FreeClient NoContent
deleteBlogF :: Token -> BlogId -> FreeClient NoContent
deleteBlogsF :: Token -> [BlogId] -> FreeClient NoContent
postBlogF :: Token -> Blog -> FreeClient (Headers '[LocationHdr] NoContent)
postBlogsF :: Token -> [Blog] -> FreeClient [BlogId]
getBlogsF :: View Be BlogT -> FreeClient (GetListHeaders Blog)
getBlogF :<|> putBlogF :<|> patchBlogF :<|> deleteBlogF :<|> deleteBlogsF :<|> postBlogF :<|> postBlogsF :<|> getBlogsF
  = Sub.client clientApi
