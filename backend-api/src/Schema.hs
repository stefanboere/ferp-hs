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

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Int                       ( Int64 )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Swagger                   ( ToParamSchema
                                                , ToSchema
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           Database.Beam
import           Database.Beam.Backend.SQL      ( HasSqlValueSyntax(..) )
import           Database.Beam.Backend.SQL.SQL92
                                                ( intType )
import           Database.Beam.Deriving
import           Database.Beam.Migrate
import           Database.Beam.Named
import           Database.Beam.Postgres         ( Postgres )
import           Database.Beam.Postgres.Syntax  ( PgValueSyntax )
import           Database.Beam.Schema.Tables    ( Ignored(..) )
import           GHC.Generics                   ( Generic )
import           Generic.Data
import           Servant.API                    ( FromHttpApiData
                                                , ToHttpApiData
                                                )
import           Servant.Crud.Server.Deriving
import           Servant.Crud.Server.QueryOperator
import           Servant.Docs

import           Database.Beam.TH               ( instances
                                                , instancesId
                                                , instancesT
                                                )

import           Types                          ( )

type instance DefaultFilters Text = StrFilter
type instance DefaultFilters String = StrFilter
type instance DefaultFilters Int = OrdFilter
type instance DefaultFilters Int64 = OrdFilter
type instance DefaultFilters Bool = EqFilter
type instance DefaultFilters (PrimaryKey t f) = EqFilter
type instance DefaultFilters (Maybe a) = AddNullFilter (DefaultFilters a)

-- * Users

-- | The full user info
data UserT f = User
  { userId       :: C f Int64
  , userEmail    :: C f Text
  , userPassword :: C f Text
  }
  deriving (Generic, Beamable)

type User = UserT Identity
type UserPatch = UserT Maybe
type UserId = PrimaryKey UserT Identity

$(instances ''UserT)
$(instancesId ''UserT ''Int64)

instance ToName UserT where
  toName = userEmail

instance ToSample User where
  toSamples _ = []

instance ToSample UserPatch where
  toSamples _ = []

instance ToSample UserId where
  toSamples _ = []

-- | Beam boilerplate
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int64) deriving (Generic, Beamable)
  primaryKey = UserId . userId

-- * UserRoles

-- | Permission level
data Role = Regular | Extra | Administrator
  deriving (Show, Read, Enum, Bounded, Eq, Ord, Generic, ToParamSchema, ToSchema)
  deriving (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON
           , FromBackendRow Postgres, HasSqlValueSyntax PgValueSyntax
           , HasSqlEqualityCheck Postgres
           ) via (BeamEnum Role)

type instance DefaultFilters Role = OrdFilter
instance HasDefaultSqlDataType Postgres Role where
  defaultSqlDataType _ _ _ = intType


class KnownRole (r :: Role) where
  roleVal :: Proxy r -> Role

instance KnownRole 'Administrator where
  roleVal _ = Administrator
instance KnownRole 'Extra where
  roleVal _ = Extra
instance KnownRole 'Regular where
  roleVal _ = Regular


-- | The full user type info
data UserRoleTT a f = UserRole
  { userRoleId   :: C f Int64
  , userRoleUser :: D a UserT f
  , userRoleRole :: C f Role
  }
  deriving (Generic, Beamable1)

-- | Beam boilerplate
instance (Beamable (UserRoleTT n), Typeable n) => Table (UserRoleTT n) where
  data PrimaryKey (UserRoleTT n) f = UserRoleId (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = UserRoleId . userRoleId

type UserRoleT = UserRoleTT PrimaryKey
type UserRole = UserRoleT Identity
type UserRoleN t = UserRoleTT Named t
type UserRoleId = PrimaryKey UserRoleT Identity

-- $(instances ''UserRoleS "_userrole")
$(instancesT ''UserRoleTT)
$(instancesId ''UserRoleT ''Int64)

instance ToSample (UserRoleTT PrimaryKey Identity) where
  toSamples _ = []

instance ToSample (UserRoleTT PrimaryKey Maybe) where
  toSamples _ = []

instance ToSample (UserRoleTT Named Identity) where
  toSamples _ = []

instance ToSample UserRoleId where
  toSamples _ = []

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
type BlogPatch = BlogT Maybe
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
data AppDatabase f = AppDatabase
  { _appDatabaseUsers     :: f (TableEntity UserT)
  , _appDatabaseUserRoles :: f (TableEntity UserRoleT)
  , _appDatabaseBlogs     :: f (TableEntity BlogT)
  }
  deriving Generic

instance Database be AppDatabase


-- | This renames the database fields
checkedAppDatabase :: CheckedDatabaseSettings Postgres AppDatabase
checkedAppDatabase =
  defaultMigratableDbSettings `withDbModification` dbModification
    { _appDatabaseUsers     = renameCheckedEntity (const "user")
    , _appDatabaseUserRoles = -- renameCheckedEntity (const "user_role")
                              modifyCheckedTable
      (const "user_role")
      ((checkedTableModification :: UserRoleT
           (CheckedFieldModification UserRoleT)
       )
        { userRoleUser = UserId $ checkedFieldNamed "user_id"
        , userRoleId   = checkedFieldNamed "id"
        }
      )
    , _appDatabaseBlogs     = renameCheckedEntity (const "blog")
    }

-- | The unchecked app database
appDatabase :: DatabaseSettings Postgres AppDatabase
appDatabase = unCheckDatabase checkedAppDatabase

-- | Information about where the foreign key point to exactly
userroleJoins :: UserRoleTT (Referenced Postgres AppDatabase) Ignored
userroleJoins = UserRole Ignored (_appDatabaseUsers appDatabase) Ignored
