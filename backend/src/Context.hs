{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module: Context
Description: Provides the information each handler can access. E.g. settings.
-}
module Context
  (
  -- * App monad
    Config(..)
  , AppConfig(..)
  , App
  , AppT(..)
  , AppServer
  )
where

import           Backend.Logger
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Dhall
import           Network.Wai.Handler.Warp       ( Port )
import           Servant
import           Servant.Auth.Server            ( JWTSettings )
import           Servant.Server                 ( ServerError )

import           OIDC

-- | App configuration which can be read (mostly) from the environment
data Config = Config
  { configInfo        :: AppInfo
  , configPort        :: Port
  , configOidc        :: OIDCConfig
  , configStaticDirectory :: FilePath
  }
  deriving Generic

instance Interpret Config

instance Interpret Int where
  autoWith cfg = fromInteger <$> autoWith cfg

-- | Convenient type alias
type AppServer api = ServerT api App

-- | Extra data for the custom monad
data AppConfig = AppConfig
  { getConfig      :: Config
  , getLogger      :: TimedFastLogger
  , getJwtSettings :: JWTSettings
  , getOIDC        :: OIDCEnv
  }

-- | Custom monad
-- see https://haskell-servant.readthedocs.io/en/stable/cookbook/hoist-server-with-context/HoistServerWithContext.html
newtype AppT m a
    = AppT
    { runApp :: ReaderT AppConfig (ExceptT ServerError m) a
    } deriving
    ( Functor, Applicative, Monad, MonadReader AppConfig, MonadError ServerError
    , MonadIO
    )

-- | The standard monad for the handlers
type App = AppT IO

-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog loc src lvl msg = do
    logger <- askLogger
    liftIO $ logger loc src lvl (toLogStr msg)

instance MonadIO m => MonadLoggerIO (AppT m) where
  askLoggerIO = askLogger

askLogger
  :: MonadIO m => AppT m (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
askLogger = do
  logger <- asks getLogger
  config <- asks getConfig
  pure $ logWithConfig logger (configInfo config)

