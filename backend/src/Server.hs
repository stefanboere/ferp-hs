{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
  )
where

import           Backend.Logger
import qualified Backend.DevelMain             as DevelMain
import           Control.Exception              ( bracket )
import           Control.Monad.Reader           ( ReaderT(..)
                                                , asks
                                                )
import           Data.Default                   ( def )
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Autohead
                                                ( autohead )
import           Network.Wai.Middleware.Gzip    ( gzip )
import qualified Network.Wai.Middleware.Timeout
                                               as Timeout
                                                ( timeout )
import           Servant
import qualified Servant.Auth.Server           as SAS
import           System.Environment             ( getArgs )

import qualified Api                            ( Api
                                                , server
                                                )
import           Context
import           OIDC

-- * Total api

type Api = Api.Api :<|> ("auth" :> IdentityApi)

api :: Proxy Api
api = Proxy

server :: AppServer Api
server = Api.server :<|> identityServer

identityServer :: AppServer IdentityApi
identityServer = handleLogin' :<|> handleLoggedIn' :<|> handleLoginFailed
 where
  handleLogin' = do
    env <- asks getOIDC
    handleLogin env

  handleLoggedIn' a b c = do
    env <- asks getOIDC
    handleLoginSuccess env a b c

-- | Registers the wai metrics and does the database migrations
initialize :: AppConfig -> Application -> IO Application
initialize cfg application = do
  -- Create request logger
  requestLogger <- initRequestLogger (getLogger cfg)
                                     (configInfo $ getConfig cfg)

  pure $ Timeout.timeout 65 $ requestLogger $ autohead $ gzip def application


type AuthContext = '[SAS.JWTSettings, SAS.CookieSettings]

-- | Bootstrap a wai app
serveApp :: AppConfig -> Application
serveApp cfg =
  let ctx = authContext cfg
  in  serveWithContext api ctx $ hoistServerWithContext
        api
        (Proxy :: Proxy AuthContext)
        (Handler . (`runReaderT` cfg) . runApp)
        server


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
  run p wai'

-- | Run this to update the app in Ghci
update :: IO ()
update = DevelMain.update app

-- | Get an application and config
testApplication :: IO Application
testApplication = bracket acquireConfig snd $ \(cfg, _) -> do
  let appl = serveApp cfg
  initialize cfg appl

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

  -- OIDC client setup
  oidc                    <- initOIDC (configOidc settings)

  -- Generate JWT key
  myKey                   <- SAS.generateKey

  pure
    ( AppConfig { getConfig      = settings
                , getLogger      = logger
                , getJwtSettings = SAS.defaultJWTSettings myKey  -- jwt Config
                , getOIDC        = oidc
                }
    , cleanupLogger
    )
 where
  cfgFile []      = "config.dhall"
  cfgFile (x : _) = x

