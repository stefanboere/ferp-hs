{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Server
Description : Provides the main executable.
              It imports an api and adds auth and documentation
-}
module Server
  ( app
  , Api
  , api
  , update
  , testApplication
  , withTestApplication
  ) where

import qualified Backend.DevelMain             as DevelMain
                                                ( update )
import           Backend.Logger
import           Control.Concurrent             ( ThreadId
                                                , killThread
                                                )
import           Control.Exception              ( bracket )
import           Control.Monad.Logger           ( LogLevel(..) )
import           Control.Monad.Metrics          ( initializeWith )
import           Control.Monad.Metrics.Internal ( _metricsStore )
import           Control.Monad.Reader           ( ReaderT(..)
                                                , ask
                                                , liftIO
                                                )
import           Data.Default                   ( def )
import           Data.Pool                      ( Pool
                                                , destroyAllResources
                                                )
import           Network.Wai.Handler.Warp
import           Network.Wai.Metrics            ( metrics
                                                , registerWaiMetrics
                                                )
import           Network.Wai.Middleware.AcceptOverride
                                                ( acceptOverride )
import           Network.Wai.Middleware.Autohead
                                                ( autohead )
import           Network.Wai.Middleware.Cors    ( CorsResourcePolicy(..)
                                                , cors
                                                , simpleCorsResourcePolicy
                                                )
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.Servant.Options
                                                ( provideOptions )
import qualified Network.Wai.Middleware.Timeout
                                               as Timeout
                                                ( timeout )
import           ProjectM36.Beamable
import           ProjectM36.Client              ( commit
                                                , relationVariablesAsRelation
                                                )
import           ProjectM36.Client.Monad        ( defineDatabase
                                                , executeIO
                                                )
import           ProjectM36.Client.Simple       ( DbConn )
import           Servant
import qualified Servant.Auth.Server           as SAS
import           Servant.Crud.Server.Middleware ( allowHeaderMiddleware )
import           Servant.Ekg                    ( monitorEndpoints )
import           System.Environment             ( getArgs )
import           System.Log.FastLogger
import           System.Remote.Monitoring       ( forkServer
                                                , serverMetricStore
                                                , serverThreadId
                                                )

import qualified Api                            ( Api
                                                , server
                                                )
import           Common.Auth
import           Context
import           Docs
import           Schema
import           WebSocket

-- * Total api

type Api = Api.Api :<|> AuthApi

api :: Proxy Api
api = Proxy

server :: AppServer Api
server = Api.server :<|> pure

-- | Registers the wai metrics and does the database migrations
initialize :: AppConfig -> Application -> IO Application
initialize cfg application = do
  -- Wai metrics
  waiMetrics      <- registerWaiMetrics (_metricsStore $ getMetric cfg)
  -- Endpoint metrics
  endpointMetrics <- monitorEndpoints api (_metricsStore $ getMetric cfg)

  -- Do schema validation
  verifyDatabase cfg

  -- Create request logger
  requestLogger <- initRequestLogger (getLogger cfg)
                                     (configInfo $ getConfig cfg)

  pure
    $ serveSubscriber' (getSubscriber cfg)
    $ Timeout.timeout 65
    $ requestLogger
    $ autohead
    $ gzip def { gzipFiles = GzipPreCompressed GzipCompress }
    $ allowHeaderMiddleware api
    $ cors (const $ Just policy)
    $ provideOptions api
    $ acceptOverride
    $ endpointMetrics
    $ metrics waiMetrics application

 where
  policy = simpleCorsResourcePolicy
    { corsRequestHeaders = ["content-type", "Authorization", "X-XSRF-TOKEN"]
    , corsExposedHeaders = Just ["Link", "Location", "X-Total-Count"]
    , corsMethods        = ["GET", "HEAD", "POST", "PUT", "PATCH", "DELETE"]
    , corsOrigins        = Just
      (unCorsOrigins . configCorsOrigins . getConfig $ cfg, True)
    }

-- | Checks the database against the schema
verifyDatabase :: AppConfig -> IO ()
verifyDatabase cfg = do
  simpleLog cfg LevelDebug "Defining database"

  verified <- runDBinIO cfg $ do
    defineDatabase appDatabase
    mapM_ executeIO textAtomFuncs

  case verified of
    Right ()  -> pure ()
    Left  err -> do
      simpleLog cfg LevelError
        .  toLogStr
        $  "The following error occured while defining the database:\n"
        <> show err

      fail "Error running migration"


type AuthContext = '[SAS.JWTSettings , SAS.CookieSettings]

-- | Combined api
type TotalApi = Api :<|> DocsApi

-- | Bootstrap a wai app
serveApp :: AppConfig -> Application
serveApp cfg =
  let ctx = authContext cfg
  in  serveWithContext totalApi' ctx
        $    hoistServerWithContext totalApi
                                    (Proxy :: Proxy AuthContext)
                                    (Handler . (`runReaderT` cfg) . runApp)
                                    totalServer
        :<|> swaggerServer cfg
 where
  -- Combined server
  totalServer :: AppServer TotalApi
  totalServer = server :<|> docsServer

  -- Proxy of the total api
  totalApi :: Proxy TotalApi
  totalApi = Proxy

  totalApi' :: Proxy (TotalApi :<|> SwaggerApi)
  totalApi' = Proxy

-- | Create an auth context
authContext :: AppConfig -> Context AuthContext
authContext cfg =
  let jwtCtx    = getJwtSettings cfg
      cookieCtx = SAS.defaultCookieSettings
        { SAS.cookieXsrfSetting = Just $ def { SAS.xsrfExcludeGet = True }
        , SAS.cookieIsSecure    = SAS.NotSecure
        } -- cookie config
  in  jwtCtx :. cookieCtx :. EmptyContext -- Context


-- | The actual web app
app :: IO ()
app = bracket acquireConfig snd $ \(cfg, _) -> do
  let wai = serveApp cfg
  wai' <- initialize cfg wai
  let p = configPort . getConfig $ cfg
  simpleLog cfg LevelDebug ("Running on port " <> toLogStr (show p))
  run p wai'

-- | Run this to update the app in Ghci
update :: IO ()
update = DevelMain.update app

-- | Get an application and config
testApplication :: IO Application
testApplication = bracket acquireConfig snd $ \(cfg, _) -> do
  let appl = serveTestApp cfg
  initialize cfg appl
 where
    -- Spin up a server (without warp). Usefull for testing. This one only has basic authentication
  serveTestApp :: AppConfig -> Application
  serveTestApp cfg = do
    let ctx = authContext cfg
    serveWithContext api ctx $ hoistServerWithContext
      api
      (Proxy :: Proxy AuthContext)
      (Handler . (`runReaderT` cfg) . runApp)
      server

-- | Spin up an application at an open port
withTestApplication :: (Port -> IO a) -> IO a
withTestApplication = withApplication testApplication

-- | Returns the config and the cleanup action for the logger and ekg server
acquireConfig :: IO (AppConfig, IO ())
acquireConfig = do
  configFile              <- cfgFile <$> getArgs

  -- Load settings from config
  settings                <- readDhallConfig configFile

  -- Setup the time cache
  (logger, cleanupLogger) <- initLoggerStdout

  -- Setup metrics
  ekgServer               <- forkServer "localhost" 3006
  let store = serverMetricStore ekgServer
  metr <- initializeWith store

  -- Get the database connection
  pool <- initConnPool (configDatabase settings)

  -- Generate JWT key
  jwt  <- generateJwtSettings (configOidcProviderUri settings)

  -- Servant subscriber
  sub  <- createSubscriber (configInfo settings) logger

  pure
    ( AppConfig { getConfig      = settings
                , getPool        = pool
                , getLogger      = logger
                , getMetric      = metr
                , getJwtSettings = jwt
                , getSubscriber  = sub
                }
    , shutdownApp cleanupLogger (serverThreadId ekgServer) pool
    )
 where
  cfgFile []      = "config.dhall"
  cfgFile (x : _) = x

  shutdownApp :: IO () -> ThreadId -> Pool DbConn -> IO ()
  shutdownApp killTimedLog ekgThread pool = do
    killTimedLog
    killThread ekgThread
    destroyAllResources pool
    pure ()
