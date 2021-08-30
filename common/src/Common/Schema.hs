{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-orphans #-}
module Common.Schema
  ( module Common.Schema
   -- * Re-exports
  , makePatch
  , purePatch
  , joinPatch
  , FieldsFulfillConstraint
  , Beamable
  , Table
  , PrimaryKey
  , primaryKey
  , C
  , Named(..)
  , ToName(..)
  ) where

import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day
                                                , UTCTime
                                                )
import           Database.Beam
import           Database.Beam.Deriving
import           Database.Beam.Expand
import           Database.Beam.Extra            ( FieldsFulfillConstraint
                                                , joinPatch
                                                , makePatch
                                                , purePatch
                                                )
import           GHC.Generics                   ( Generic )
import           Generic.Data
import           Servant.Crud.Deriving
import           Servant.Crud.QueryOperator

import           Common.Schema.TH

type instance DefaultFilters Text = StrFilter
type instance DefaultFilters String = StrFilter
type instance DefaultFilters Int = OrdFilter
type instance DefaultFilters Int64 = OrdFilter
type instance DefaultFilters Bool = EqFilter
type instance DefaultFilters (PrimaryKey t f) = EqFilter
type instance DefaultFilters (Maybe a) = AddNullFilter (DefaultFilters a)
type instance DefaultFilters Day = OrdFilter
type instance DefaultFilters UTCTime = OrdFilter

-- | The full user blog info
data BlogT f = Blog
  { _blogId          :: C f Int64
  , _blogName        :: C f Text
  , _blogDescription :: C f Text
  , _blogIsExtra     :: C f Bool
  , _blogIsPublished :: C f Bool
  , _blogDate        :: C f Day
  }
  deriving (Generic, Beamable)


type Blog = BlogT Identity
type BlogPatch = BlogT MaybeLast
type BlogId = PrimaryKey BlogT Identity

{- HLINT ignore "Redundant bracket" -}
$(instances ''BlogT)
{- HLINT ignore "Redundant bracket" -}
$(instancesId ''BlogT ''Int64)

instance ToName BlogT where
  toName = _blogName

-- | Beam boilerplate
instance Table BlogT where
  data PrimaryKey BlogT f = BlogId (Columnar f Int64) deriving (Generic, Beamable)
  primaryKey = BlogId . _blogId

-- | A group of related blog posts
data ChannelT f = Channel
  { _channelId   :: C f Int64
  , _channelName :: C f Text
  }
  deriving (Generic, Beamable)

type Channel = ChannelT Identity
type ChannelPatch = ChannelT MaybeLast
type ChannelId = PrimaryKey ChannelT Identity

{- HLINT ignore "Redundant bracket" -}
$(instances ''ChannelT)
{- HLINT ignore "Redundant bracket" -}
$(instancesId ''ChannelT ''Int64)

instance ToName ChannelT where
  toName = _channelName

instance Table ChannelT where
  data PrimaryKey ChannelT f = ChannelId (Columnar f Int64) deriving (Generic, Beamable)
  primaryKey = ChannelId . _channelId

