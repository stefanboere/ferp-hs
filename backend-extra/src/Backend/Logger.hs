{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Backend.Logger
Description: Utilities to setup logging
-}
module Backend.Logger
  ( AppInfo(..)
  , readDhallConfig
  , initRequestLogger
  , skipLowerPrefixInterpretOptions
  -- * Logging
  , initLoggerStdout
  , logWithConfig
  , Environment(..)
  -- * Re-exports
  , TimedFastLogger
  , LogLevel(..)
  )
where

import           Control.Monad.Logger
import           Data.Char                      ( isLower
                                                , toLower
                                                )
import           Data.Default                   ( def )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Version
import           Dhall
import           Network.Wai                    ( Middleware )
import           Network.Wai.Middleware.RequestLogger
                                                ( Destination(..)
                                                , destination
                                                , mkRequestLogger
                                                )
import           System.Log.FastLogger
import           Text.ParserCombinators.ReadP   ( readP_to_S )
import           Text.Read                      ( readMaybe )

initRequestLogger :: TimedFastLogger -> AppInfo -> IO Middleware
initRequestLogger logger cfg = mkRequestLogger
  $ def { destination = Callback $ simpleLog logger cfg LevelDebug }


initLoggerStdout :: IO (TimedFastLogger, IO ())
initLoggerStdout = do
  -- Setup the time cache
  timeCache <- newTimeCache simpleTimeFormat
  newTimedFastLogger timeCache logStdout

readDhallConfig :: FromDhall a => String -> IO a
readDhallConfig = inputFile auto

skipLowerPrefixInterpretOptions :: InterpretOptions
skipLowerPrefixInterpretOptions = defaultInterpretOptions
  { fieldModifier = decapitalize . Text.dropWhile isLower
  }
 where
  decapitalize x = case Text.uncons x of
    Just (z, zs) -> Text.cons (toLower z) zs
    Nothing      -> Text.empty


data AppInfo = AppInfo
  { appTitle       :: Text
  , appDescription :: Text
  , appVersion     :: Version
  , appEnvironment :: Environment
  , appLogLevel    :: LogLevel
  }
  deriving Generic

instance FromDhall AppInfo where
  autoWith _ = genericAutoWith skipLowerPrefixInterpretOptions

instance FromDhall Version where
  autoWith cfg = either error id . readVersion <$> autoWith cfg

readVersion :: String -> Either String Version
readVersion s = case [ x | (x, "") <- readP_to_S parseVersion s ] of
  [x] -> Right x
  _   -> Left $ "Could not parse version " <> s

-- | Either 'Test', 'Development' or 'Production'
data Environment = Production | Development | Test deriving (Show, Eq, Read, Ord)

instance FromDhall Environment where
  autoWith cfg =
    fromMaybe (error "Could not read Environment") . readMaybe <$> autoWith cfg

readLogLevel :: String -> LogLevel
readLogLevel str = case readMaybe str of
  Just x  -> x
  Nothing -> LevelOther (Text.pack str)

instance FromDhall LogLevel where
  autoWith cfg = autoWith cfg <&> readLogLevel

logWithConfig
  :: TimedFastLogger
  -> AppInfo
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
logWithConfig logger appInfo = \loc src lvl msg ->
  if appLogLevel appInfo <= lvl
    then logger (logStr appInfo loc src lvl msg)
    else pure ()
 where
  logStr
    :: AppInfo
    -> Loc
    -> LogSource
    -> LogLevel
    -> LogStr
    -> FormattedTime
    -> LogStr
  logStr cfg loc src lvl msg now = toLogStr now <> " " <> defaultLogStr
    loc
    src
    lvl
    (envLogStr cfg <> ": " <> msg)

  envLogStr :: AppInfo -> LogStr
  envLogStr cfg =
    "{"
      <> toLogStr (showVersion . appVersion $ cfg)
      <> "-"
      <> toLogStr (take 4 . show . appEnvironment $ cfg)
      <> "}"


-- | Simple standalone logger. Only needs the AppConfig
simpleLog :: TimedFastLogger -> AppInfo -> LogLevel -> LogStr -> IO ()
simpleLog logger cfg = logWithConfig logger cfg defaultLoc ""

-- | Logger which logs to the stdout
logStdout :: LogType
logStdout = LogStdout defaultBufSize
