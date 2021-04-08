{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module: Database.Beam.Deriving
Description: Utilities to automatically derive all kinds of type classes with DerivingVia

This module provides newtypes which can be used to derive instances with the -XDerivingVia
language extension.

-}
module Database.Beam.Deriving
  ( BeamOrderBy(..)
  , BeamEnum(..)
  )
where

import           Prelude

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , withText
                                                )
import           Data.Functor.Contravariant     ( contramap )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Text                     as Text
import           Data.Typeable                  ( Typeable )
import           Database.Beam                  ( HasSqlEqualityCheck )
import           Database.Beam.API              ( Orderable )
import           Database.Beam.Backend.SQL      ( BeamSqlBackend
                                                , FromBackendRow(..)
                                                , HasSqlValueSyntax(..)
                                                )
import           Database.Beam.Backend.Types    ( BeamBackend )
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           Servant.API
import           Servant.Crud.Deriving          ( queryOptions )
import           Servant.Crud.OrderBy           ( GSelectors
                                                , Selectors(..)
                                                , defaultSelectors
                                                )
import           Text.Read                      ( readEither )


-- | Newtype for usage with -XDeriveVia to derive instances 'Selectors', using
-- 'queryOptions'  instead of the default options.
newtype BeamOrderBy a = BeamOrderBy { unBeamOrderBy :: a }

-- | A common type name prefix is dropped
instance ( BeamSqlBackend be, Typeable r, Generic r, GSelectors (Orderable be) (Rep r))
      => Selectors (Orderable be) (BeamOrderBy r) where
  selectors = fmap (contramap unBeamOrderBy)
    <$> defaultSelectors (queryOptions (Proxy :: Proxy r))


-- | Newtype for usage with -XDeriveVia to derive instances for beam for enum types
--
-- > {-# LANGUAGE DerivingVia #-}
-- > ...
-- > data Quality = Good | Bad | Ugly
-- >     deriving (Read, Show, Bounded, Enum, Eq, Ord)
-- >       deriving ( FromHttpApiData, ToHttpApiData, FromJSON, ToJSON
-- >                , HasDefaultSqlDataType Postgres, FromBackendRow Postgres
-- >                , HasSqlValueSyntax PgValueSyntax, HasSqlEqualityCheck Postgres
-- >                ) via (BeamEnum Role)
newtype BeamEnum a = BeamEnum { unBeamEnum :: a }


instance Read a => FromHttpApiData (BeamEnum a) where
  parseQueryParam x =
    parseQueryParam x
      >>= (either (Left . Text.pack) (Right . BeamEnum) . readEither)

instance Show a => ToHttpApiData (BeamEnum a) where
  toQueryParam = toQueryParam . Text.pack . show . unBeamEnum

instance Read a => FromJSON (BeamEnum a) where
  parseJSON =
    withText "Role"
      $ either (fail "Invalid role") (pure . BeamEnum)
      . readEither
      . Text.unpack

instance Show a => ToJSON (BeamEnum a) where
  toJSON = toJSON . Text.pack . show . unBeamEnum

-- instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be (BeamEnum a) where
--  defaultSqlDataType _ _ _ = intType
-- TODO

instance (BeamBackend be, FromBackendRow be Integer, Enum a) => FromBackendRow be (BeamEnum a) where
  fromBackendRow = BeamEnum . toEnum . fromInteger <$> fromBackendRow

instance (Enum a, HasSqlValueSyntax expr Integer) => HasSqlValueSyntax expr (BeamEnum a) where
  sqlValueSyntax = sqlValueSyntax . toInteger . fromEnum . unBeamEnum

instance BeamSqlBackend expr => HasSqlEqualityCheck expr (BeamEnum a)
