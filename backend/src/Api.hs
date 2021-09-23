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
                                                , ReaderT(..)
                                                )
import           Data.Aeson                     ( encode )
import           Data.ByteString                ( ByteString )
import qualified Frontend
import           Lucid
import           Lucid.Base                     ( makeAttribute )
import           Reflex.Dom
import           Servant                 hiding ( URI(..) )
import           Servant.Crud.Server.QueryObject
                                                ( )
import           Servant.RawM                  as RawM
import           Servant.Router
import           URI.ByteString

import           Auth                           ( )
import           Context


-- | The api
-- brittany-disable-next-binding
type Api = "favicon.ico" :> Get '[JSON] NoContent
  :<|> "static" :> RawM
  :<|> Frontend.Api

-- | A proxy of the api
api :: Proxy Api
api = Proxy

-- | The combined server
server :: AppServer Api
server = faviconEndpoint :<|> staticEndpoint :<|> frontendServer

faviconEndpoint :: AppServer (Get '[] NoContent)
faviconEndpoint =
  throwError err302 { errHeaders = [("Location", "/static/favicon.ico")] }

staticEndpoint :: AppServer RawM
staticEndpoint =
  asks getConfig >>= RawM.serveDirectoryWebApp . configStaticDirectory


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
  cfg <- asks getConfig
  let page = runReaderT pageT (configFrontend cfg)
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
        script_ [type_ "text/plain", id_ "config"] (encode (configFrontend cfg))
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
