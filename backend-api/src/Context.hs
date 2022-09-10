{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  , AppInfo(..)
  , App
  , AppT(..)
  , DBE(..)
  , AppServer
  , CorsOrigins(..)
  -- * Database
  , initConnPool
  , runDB
  , runDBinIO
   -- * Logging
  , simpleLog
  -- * JWT
  , discoverJWKs
  , generateJwtSettings
  ) where

import           Backend.Logger
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , runExceptT
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Logger
import           Control.Monad.Metrics          ( Metrics
                                                , MonadMetrics
                                                , getMetrics
                                                )
import           Control.Monad.Reader
import qualified Crypto.JOSE                   as Jose
import           Data.Aeson
import           Data.Maybe                     ( fromMaybe )
import           Data.Pool
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Dhall                   hiding ( maybe )
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import qualified Network.URI                   as Network
import           Network.Wai.Handler.Warp       ( Port )
import           Network.Wai.Middleware.Cors    ( Origin )
import           ProjectM36.Client              ( DatabaseName
                                                , Hostname
                                                , ServiceName
                                                , TransactionGraphOperator(..)
                                                , executeGraphExpr
                                                )
import qualified ProjectM36.Client.Monad       as Monad
                                                ( DbConn )
import           ProjectM36.Client.Simple
import           Servant
import           Servant.Auth.Server            ( JWTSettings
                                                , defaultJWTSettings
                                                , generateKey
                                                , validationKeys
                                                )
import           Servant.Server                 ( ServerError )
import           Servant.Subscriber

import           Common.Api

data ConnectInfo = InProcessConnectInfo PersistenceStrategy
                 | RemoteConnectInfo DatabaseName Hostname ServiceName
                 deriving (Generic)

-- | App configuration which can be read (mostly) from the environment
data Config = Config
  { configInfo            :: AppInfo
  , configDatabase        :: ConnectInfo
  , configPort            :: Port
  , configCorsOrigins     :: CorsOrigins
  , configOidcProviderUri :: URI
  }
  deriving Generic

instance FromDhall Config where
  autoWith _ = genericAutoWith skipLowerPrefixInterpretOptions

deriving instance Generic PersistenceStrategy

instance FromDhall PersistenceStrategy where
  autoWith _ = genericAutoWith skipLowerPrefixInterpretOptions

instance FromDhall ConnectInfo where
  autoWith _ = genericAutoWith skipLowerPrefixInterpretOptions

instance FromDhall Network.URI where
  autoWith cfg = go <$> autoWith cfg
   where
    go :: Text -> Network.URI
    go = fromMaybe (error "") . Network.parseURI . Text.unpack

newtype CorsOrigins = CorsOrigins { unCorsOrigins :: [Origin] }

instance FromDhall CorsOrigins where
  autoWith cfg = CorsOrigins <$> autoWith cfg

instance FromDhall Origin where
  autoWith cfg = Text.encodeUtf8 <$> autoWith cfg



-- | Convenient type alias
type AppServer api = ServerT api App

-- | Extra data for the custom monad
data AppConfig = AppConfig
  { getConfig      :: Config
  , getPool        :: Pool DbConn
  , getLogger      :: TimedFastLogger
  , getMetric      :: Metrics
  , getJwtSettings :: JWTSettings
  , getSubscriber  :: Subscriber Api
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

-- | Simple standalone logger. Only needs the AppConfig
simpleLog :: AppConfig -> LogLevel -> LogStr -> IO ()
simpleLog cfg =
  logWithConfig (getLogger cfg) (configInfo $ getConfig cfg) defaultLoc ""


instance Monad m => MonadMetrics (AppT m) where
  getMetrics = asks getMetric

-- DATABASE

-- | Creates a pool from a connect info
initConnPool :: ConnectInfo -> IO (Pool DbConn)
initConnPool info = createPool (connect (toConnectionInfo info))
                               close
                               2 -- stripes
                               60 -- unused connections are open for a minute
                               10 -- max. 10 connections per open stripe
 where
  connect x = simpleConnectProjectM36 x >>= either (fail . show) pure
  toConnectionInfo (InProcessConnectInfo s) =
    InProcessConnectionInfo s callback []
  toConnectionInfo (RemoteConnectInfo d h s) =
    RemoteConnectionInfo d h s callback

  callback _ _ = pure ()


-- * Various utilities

newtype DBE a = DBE { runDBE :: ReaderT Monad.DbConn (ExceptT RelationalError Db) a }
    deriving
    ( Functor, Applicative, Monad, MonadError RelationalError, MonadReader Monad.DbConn, MonadIO)

-- | Gets the pool and applies it to your function
runDB :: (MonadReader AppConfig m, MonadIO m) => DBE b -> m b
runDB q = do
  pool <- asks getPool
  r    <- liftIO $ withResource pool $ \conn ->
    runProjectM36 conn (runExceptT $ runReaderT (runDBE q) conn)
  either (fail . show) pure r

-- | Runs the database in the IO monad
runDBinIO :: AppConfig -> DBE b -> IO (Either DbError b)
runDBinIO cfg q = do
  let pool = getPool cfg
  r <- liftIO $ withResource pool $ \conn ->
    withTransaction conn (runExceptT $ runReaderT (runDBE q) conn)
  pure $ case r of
    Right (Left  x) -> Left (RelError x)
    Right (Right x) -> Right x
    Left  y         -> Left y


runProjectM36 :: DbConn -> Db b -> IO b
runProjectM36 dbConn q = do
  x <- withTransaction dbConn q
  either (fail . show) pure x

-- * JWT

-- | Discover the JWK keys of the openid connect provider
discoverJWKs :: URI -> IO Jose.JWKSet
discoverJWKs uri = do
  manager <- Network.HTTP.Client.newManager tlsManagerSettings

  cfgReq  <- requestFromURI uri
    { uriPath = uriPath uri <> ".well-known/openid-configuration"
    }
  cfgResp  <- httpLbs cfgReq manager
  jwksUri  <- either fail pure $ eitherDecode (responseBody cfgResp)

  jwksReq  <- requestFromURI (jwks_uri jwksUri)
  jwksResp <- httpLbs jwksReq manager
  either fail pure $ eitherDecode (responseBody jwksResp)

generateJwtSettings :: URI -> IO JWTSettings
generateJwtSettings uri = do
  myKey <- generateKey
  jwks  <- discoverJWKs uri
  pure $ (defaultJWTSettings myKey) { validationKeys = jwks }

newtype JWKSUri = JWKSUri { jwks_uri :: URI } deriving (Eq, Show, Generic)

instance FromJSON JWKSUri

