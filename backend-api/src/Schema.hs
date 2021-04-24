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
module Schema where

import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           Database.Beam
import           Database.Beam.Deriving
import           Database.Beam.Migrate
import           Database.Beam.Named
import           Database.Beam.Postgres         ( Postgres )
import           GHC.Generics                   ( Generic )
import           Generic.Data
import           Servant.Crud.Server.Deriving
import           Servant.Crud.Server.QueryOperator
import           Servant.Docs

import           Database.Beam.TH               ( instances
                                                , instancesId
                                                )

import           Types                          ( )

type instance DefaultFilters Text = StrFilter
type instance DefaultFilters String = StrFilter
type instance DefaultFilters Int = OrdFilter
type instance DefaultFilters Int64 = OrdFilter
type instance DefaultFilters Bool = EqFilter
type instance DefaultFilters (PrimaryKey t f) = EqFilter
type instance DefaultFilters (Maybe a) = AddNullFilter (DefaultFilters a)

-- * Blog

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

instance ToSample Blog where
  toSamples _ = []

instance ToSample BlogPatch where
  toSamples _ = []

instance ToSample BlogId where
  toSamples _ = []

-- | Beam boilerplate
instance Table BlogT where
  data PrimaryKey BlogT f = BlogId (Columnar f Int64) deriving (Generic, Beamable)
  primaryKey = BlogId . blogId

-- * The Database

-- | Here live all the tables in the database
newtype AppDatabase f = AppDatabase
  { _appDatabaseBlogs     :: f (TableEntity BlogT)
  }
  deriving Generic

instance Database be AppDatabase


-- | This renames the database fields
checkedAppDatabase :: CheckedDatabaseSettings Postgres AppDatabase
checkedAppDatabase =
  defaultMigratableDbSettings `withDbModification` dbModification
    { _appDatabaseBlogs = renameCheckedEntity (const "blog")
    }

-- | The unchecked app database
appDatabase :: DatabaseSettings Postgres AppDatabase
appDatabase = unCheckDatabase checkedAppDatabase
