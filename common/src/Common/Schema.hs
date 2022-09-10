{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-orphans #-}
module Common.Schema
  ( module Common.Schema
  , module Common.Types
   -- * Re-exports
  , makePatch
  , purePatch
  , joinPatch
  , flattenNamed
  , FieldsFulfillConstraint
  , Beamable
  , Table
  , PrimaryKey
  , primaryKey
  , C
  , Named(..)
  , ToName(..)
  , TableT(..)
  ) where

import           Data.Functor.Identity          ( Identity )
import           Data.Monoid                    ( Last )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day
                                                , UTCTime
                                                )
import           GHC.Generics                   ( Generic )
import           Generic.Data
import           ProjectM36.Beamable
import           Servant.Crud.Deriving
import           Servant.Crud.QueryOperator

import           Common.Schema.TH
import           Common.Types


type instance DefaultFilters Text = StrFilter
type instance DefaultFilters String = StrFilter
type instance DefaultFilters Int = OrdFilter
type instance DefaultFilters Integer = OrdFilter
type instance DefaultFilters Bool = EqFilter
type instance DefaultFilters (PrimaryKey t f) = EqFilter
type instance DefaultFilters (Maybe a) = AddNullFilter (DefaultFilters a)
type instance DefaultFilters Day = OrdFilter
type instance DefaultFilters UTCTime = OrdFilter

-- | A group of related blog posts
data ChannelT f = Channel
  { _channelId   :: C f Integer
  , _channelName :: C f Text
  }
  deriving (Generic, Beamable)

type Channel = ChannelT Identity
type ChannelPatch = ChannelT Last
type ChannelId = PrimaryKey ChannelT Identity

{- HLINT ignore "Redundant bracket" -}
$(instances ''ChannelT)
{- HLINT ignore "Redundant bracket" -}
$(instancesId ''ChannelT ''Integer)

instance ToName ChannelT where
  toName _ = AttributeAtomExpr "_channelName"

instance Table ChannelT where
  data PrimaryKey ChannelT f = ChannelId (Columnar f Integer) deriving (Generic, Beamable)
  primaryKey = ChannelId . _channelId

instance TableT ChannelT where
  type BaseTable ChannelT = ChannelT
  getPrimaryKey = primaryKey

-- | The full user blog info
data BlogTT g f = Blog
  { _blogId          :: C f Integer
  , _blogChannel     :: D g ChannelT f
  , _blogName        :: C f Text
  , _blogDescription :: C f Text
  , _blogIsExtra     :: C f Bool
  , _blogIsPublished :: C f Bool
  , _blogDate        :: C f Day
  }
  deriving (Generic, Beamable1)

type BlogN = BlogTT Named
type BlogN1 = BlogN Identity
type BlogT = BlogTT PrimaryKey
type Blog = BlogT Identity
type BlogPatch = BlogT Last
type BlogId = PrimaryKey BlogT Identity

{- HLINT ignore "Redundant bracket" -}
$(instancesT ''BlogTT)

instance ToName BlogT where
  toName _ = AttributeAtomExpr "_blogName"

-- | Beam boilerplate
instance Table BlogT  where
  data PrimaryKey BlogT f = BlogId (Columnar f Integer) deriving (Generic, Beamable)
  primaryKey = BlogId . _blogId

instance TableT BlogT where
  type BaseTable BlogT = BlogT
  getPrimaryKey = primaryKey

{- HLINT ignore "Redundant bracket" -}
$(instancesId ''BlogT ''Integer)
