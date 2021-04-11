{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types
  ()
where

import           Data.Csv
import           Data.Time                      ( Day
                                                , UTCTime
                                                , formatTime
                                                , defaultTimeLocale
                                                )
import           Servant.Crud.Server.QueryOperator
                                                ( DefaultFilters
                                                , OrdFilter
                                                )
-- * Date orphans

type instance DefaultFilters Day = OrdFilter
type instance DefaultFilters UTCTime = OrdFilter


instance ToField Bool where
  toField True  = "true"
  toField False = "false"

instance ToField Day where
  toField = toField . formatTime defaultTimeLocale "%F"
