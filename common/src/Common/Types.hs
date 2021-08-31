{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Common.Types
  ( SerialInt64
  ) where

import           Data.Int                       ( Int64 )
import           Database.Beam.Backend.SQL.Types
                                                ( SqlSerial(..) )
import           Servant.API                    ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )

type SerialInt64 = SqlSerial Int64

deriving newtype instance FromHttpApiData a => FromHttpApiData (SqlSerial a)
deriving newtype instance ToHttpApiData a => ToHttpApiData (SqlSerial a)
