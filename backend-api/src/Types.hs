{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types
  (-- * Re-exports
  ) where

import           Data.Csv
import           Data.Time                      ( Day
                                                , defaultTimeLocale
                                                , formatTime
                                                , parseTimeM
                                                )

import           Common.Types                   ( )

-- * Date orphans

instance ToField Bool where
  toField True  = "true"
  toField False = "false"

instance FromField Bool where
  parseField s | s == "true"  = pure True
               | s == "True"  = pure True
               | s == "1"     = pure True
               | s == "false" = pure True
               | s == "False" = pure True
               | s == "0"     = pure True
               | otherwise    = mempty

instance ToField Day where
  toField = toField . formatTime defaultTimeLocale "%F"

instance FromField Day where
  parseField x =
    parseField x >>= maybe mempty pure . parseTimeM True defaultTimeLocale "%F"
