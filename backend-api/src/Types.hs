{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types
  (-- * Re-exports
    SqlSerial(..)
  )
where

import           Data.Csv
import           Data.Swagger                   ( ToParamSchema
                                                , ToSchema
                                                )
import           Data.Time                      ( Day
                                                , defaultTimeLocale
                                                , formatTime
                                                , parseTimeM
                                                )
import           Database.Beam.Backend.SQL.Types
                                                ( SqlSerial(..) )

import           Common.Types                   ( )

deriving newtype instance ToSchema a => ToSchema (SqlSerial a)
deriving newtype instance ToParamSchema a => ToParamSchema (SqlSerial a)
deriving newtype instance ToField a => ToField (SqlSerial a)
deriving newtype instance FromField a => FromField (SqlSerial a)

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
