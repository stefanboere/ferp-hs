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

import           Data.Functor.Identity          ( Identity )
import           Data.Proxy                     ( Proxy(..) )
import           GHC.Generics                   ( Generic )
import           ProjectM36.Beamable
import           Servant.Docs

import           Database.Beam.TH               ( instances
                                                , instancesId
                                                , instancesT
                                                )
import           Servant.Crud.Server.Deriving   ( CsvBody(..) )

import           Common.Schema
import           Types                          ( )

-- * Blog

{- HLINT ignore "Redundant bracket" -}
$(instancesT ''BlogTT)
{- HLINT ignore "Redundant bracket" -}
$(instancesId ''BlogT ''Integer)

instance ToSample Blog where
  toSamples _ = []

instance ToSample BlogPatch where
  toSamples _ = []

instance ToSample BlogId where
  toSamples _ = []

instance ToSample (BlogN Identity) where
  toSamples _ = []

instance ToSample (PrimaryKey BlogN Identity) where
  toSamples _ = []

-- * Channel

{- HLINT ignore "Redundant bracket" -}
$(instances ''ChannelT)
{- HLINT ignore "Redundant bracket" -}
$(instancesId ''ChannelT ''Integer)

instance ToSample Channel where
  toSamples _ = []

instance ToSample ChannelPatch where
  toSamples _ = []

instance ToSample ChannelId where
  toSamples _ = []


-- * The Database

-- | Here live all the tables in the database
data AppDatabaseT f = AppDatabase
  { _appDatabaseBlogs    :: C f (Proxy BlogT)
  , _appDatabaseChannels :: C f (Proxy ChannelT)
  }
  deriving (Generic, Beamable)

type AppDatabase = AppDatabaseT Identity

appDatabase :: AppDatabase
appDatabase = AppDatabase Proxy Proxy
