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
module Common.Schema where

import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day
                                                , UTCTime
                                                )
import           Database.Beam
import           Database.Beam.Deriving
import           Database.Beam.Expand
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
  { blogId          :: C f Int64
  , blogName        :: C f Text
  , blogDescription :: C f Text
  , blogIsExtra     :: C f Bool
  , blogIsPublished :: C f Bool
  , blogDate        :: C f Day
  }
  deriving (Generic, Beamable)


type Blog = BlogT Identity
type BlogPatch = BlogT MaybeLast
type BlogId = PrimaryKey BlogT Identity

$(instances ''BlogT)
$(instancesId ''BlogT ''Int64)

instance ToName BlogT where
  toName = blogName

-- | Beam boilerplate
instance Table BlogT where
  data PrimaryKey BlogT f = BlogId (Columnar f Int64) deriving (Generic, Beamable)
  primaryKey = BlogId . blogId
