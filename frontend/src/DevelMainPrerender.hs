{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module DevelMainPrerender
  ( app
  )
where


import           Control.Monad.Except           ( ExceptT )
import           Control.Monad.Reader           ( ReaderT(..)
                                                , ask
                                                , liftIO
                                                )
import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy          as BL
import           Data.ByteString                ( ByteString )
import           Lucid
import           Lucid.Base                     ( makeAttribute )
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Network.Wai.Handler.Warp
import           Reflex.Dom
import           Servant                 hiding ( URI(..) )
import           Servant.Router
import           URI.ByteString
import           Servant.Crud.Server.QueryObject
                                                ( )

import           Frontend
import           Common.Auth

type App = ReaderT Config (ExceptT ServerError IO)

frontendServer :: RouteT Frontend.Api App (Html ())
frontendServer = hoistRoute Frontend.api prerenderApp Frontend.handlerOffline

-- | The transformation converting the frontend to the backend
prerenderApp
  :: ReaderT
       Frontend.Config
       (StaticWidget x)
       (Event (SpiderTimeline Global) URI)
  -> App (Html ())
prerenderApp pageT = do
  cfg <- ask
  let page = runReaderT pageT cfg
  (_, body) <- liftIO $ renderStatic (Frontend.withHeader (page' page))
  pure $ do
    doctype_
    html_ [lang_ "en"] $ do
      head_ $ do
        title_ "Ferp-hs"
        meta_ [charset_ "UTF-8"]
        meta_
          [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        link_ [rel_ "shortcut icon", href_ "/static/favicon.ico"]
        link_ [href_ "/static/style.css", rel_ "stylesheet", type_ "text/css"]
        link_
          [ href_ "/static/all.min.js"
          , rel_ "preload"
          , makeAttribute "as" "script"
          ]
        script_ [type_ "text/plain"] (encode cfg)
        mapM_ asyncScript
              ["/static/vendor/ace/ace.js", "/static/vendor/tex-chtml.min.js"]
      body_ $ do
        toHtmlRaw body
        script_
          [src_ "/static/all.min.js", type_ "text/javascript", defer_ "defer"]
          ("" :: ByteString)

 where
  asyncScript src =
    script_ [src_ src, type_ "text/javascript"] ("" :: ByteString)

  page' page _ = do
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

-- | The actual web app
app :: IO ()
app = do
  cfg <- getConfigFromFile "config.json"
  let wai = serve
        Frontend.api
        (hoistServer Frontend.api (naturalTrans cfg) frontendServer)
  Network.Wai.Handler.Warp.run 3009 wai

 where
  naturalTrans :: Config -> App a -> Handler a
  naturalTrans cfg r = Handler $ runReaderT r cfg

instance (HasServer api ctxs)
  => HasServer (Common.Auth.Auth r :> api) ctxs where
  type ServerT (Common.Auth.Auth r :> api) m = ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt s

  route _ context subserver =
    Servant.route (Proxy :: Proxy api) context subserver

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ = BL.fromStrict

instance MimeRender HTML (Html ()) where
  mimeRender _ = renderBS

instance HasServer View context where
  type ServerT View m = m (Html ())
  hoistServerWithContext _ _ nt s = nt s

  route Proxy cnt action =
    Servant.route (Proxy :: Proxy (Get '[HTML] (Html ()))) cnt action
