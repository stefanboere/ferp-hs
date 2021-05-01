{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
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

import           Control.Monad.Reader           ( asks
                                                , liftIO
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as BL
                                                ( fromStrict )
import qualified Frontend
import           Lucid
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Reflex.Dom
import           Servant                 hiding ( URI(..) )
import           Servant.RawM                  as RawM
import           Servant.Router
import           URI.ByteString

import           Context

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ = BL.fromStrict

instance MimeRender HTML (Html ()) where
  mimeRender _ = renderBS

-- | The api
-- brittany-disable-next-binding
type Api = "static" :> RawM
  :<|> Frontend.Api

-- | A proxy of the api
api :: Proxy Api
api = Proxy

-- | The combined server
server :: AppServer Api
server = staticEndpoint :<|> frontendServer

staticEndpoint :: AppServer RawM
staticEndpoint =
  asks getConfig >>= RawM.serveDirectoryWebApp . configStaticDirectory


frontendServer :: RouteT Frontend.Api App (Html ())
frontendServer = hoistRoute Frontend.api prerenderApp Frontend.handler

-- | The transformation converting the frontend to the backend
prerenderApp
  :: StaticWidget x (Event (SpiderTimeline Global) URI) -> App (Html ())
prerenderApp page = do
  (_, body) <- liftIO $ renderStatic (Frontend.withHeader page')
  pure $ doctypehtml_ $ do
    head_ $ do
      link_ [href_ "/static/style.css", rel_ "stylesheet", type_ "text/css"]
      script_ [src_ "/static/all.min.js"] ("" :: ByteString)
    body_ $ toHtmlRaw body
 where
  page' _ = do
    x <- page
    holdDyn
      (URI { uriScheme    = Scheme "http"
           , uriAuthority = Nothing
           , uriPath      = ""
           , uriQuery     = Query []
           , uriFragment  = Nothing
           }
      )
      x

instance HasServer View context where
  type ServerT View m = m (Html ())
  hoistServerWithContext _ _ nt s = nt s

  route Proxy cnt action =
    Servant.route (Proxy :: Proxy (Get '[HTML] (Html ()))) cnt action
