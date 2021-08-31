{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-export-lists -Wno-orphans #-}
{-|
Module: Schema
Description: This module defines the database schema
-}
module Schema
  ( module Common.Schema
  , module Schema
  ) where

import           Database.Beam
import           Database.Beam.Migrate
import           Database.Beam.Postgres         ( Postgres )
import           GHC.Generics                   ( Generic )
import           Servant.Docs

import           Database.Beam.TH               ( instances
                                                , instancesId
                                                )
import           Servant.Crud.Server.Deriving   ( CsvBody(..) )

import           Common.Schema
import           Types                          ( )

-- * Blog

{- HLINT ignore "Redundant bracket" -}
$(instances ''BlogT)
{- HLINT ignore "Redundant bracket" -}
$(instancesId ''BlogT ''SerialInt64)

instance ToSample Blog where
  toSamples _ = []

instance ToSample BlogPatch where
  toSamples _ = []

instance ToSample BlogId where
  toSamples _ = []

-- * Channel

{- HLINT ignore "Redundant bracket" -}
$(instances ''ChannelT)
{- HLINT ignore "Redundant bracket" -}
$(instancesId ''ChannelT ''SerialInt64)

instance ToSample Channel where
  toSamples _ = []

instance ToSample ChannelPatch where
  toSamples _ = []

instance ToSample ChannelId where
  toSamples _ = []


-- * The Database

-- | Here live all the tables in the database
data AppDatabase f = AppDatabase
  { _appDatabaseBlogs    :: f (TableEntity BlogT)
  , _appDatabaseChannels :: f (TableEntity ChannelT)
  }
  deriving Generic

instance Database be AppDatabase


-- | This renames the database fields
checkedAppDatabase :: CheckedDatabaseSettings Postgres AppDatabase
checkedAppDatabase =
  defaultMigratableDbSettings `withDbModification` dbModification
    { _appDatabaseBlogs    = renameCheckedEntity (const "blog")
    , _appDatabaseChannels = renameCheckedEntity (const "channel")
    }

-- | The unchecked app database
appDatabase :: DatabaseSettings Postgres AppDatabase
appDatabase = unCheckDatabase checkedAppDatabase
