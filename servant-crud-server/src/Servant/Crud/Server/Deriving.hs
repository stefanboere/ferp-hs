{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Servant.Crud.Server.Deriving
Description: Utilities to automatically derive all kinds of type classes with DerivingVia

This module provides newtypes which can be used to derive instances with the -XDerivingVia
language extension.

-}
module Servant.Crud.Server.Deriving
  ( module Servant.Crud.Deriving
  , CsvBody(..)
  , GDefaultOrdered
  , GFromNamedRecord
  , GToNamedRecord
  ) where

import           Prelude

import           Data.ByteString                ( ByteString )
import qualified Data.Csv                      as Csv
import           Data.Proxy                     ( Proxy(..) )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           Servant.Crud.Deriving
import           Servant.Crud.Server.QueryObject
                                                ( GToParams
                                                , ToParams(..)
                                                , defaultToParams
                                                )

-- | A common type name prefix is dropped
instance (Generic a, Typeable a, GToParams lang ftype (Rep a))
    => ToParams lang ftype (QueryType a) where
  toParams t lang ftype _ = defaultToParams (queryOptions (Proxy :: Proxy a))
                                            t
                                            lang
                                            ftype
                                            (Proxy :: Proxy a)

-- | Newtype for usage with -XDeriveVia to derive instances used for types which are
-- used in request/response bodies and need Csv content type output.
--
-- > deriving via (CsvBody $(t)) instance FromNamedRecord $(t)
newtype CsvBody a = CsvBody { unCsvBody :: a }


csvOptions :: Typeable a => Proxy a -> Csv.Options
csvOptions p =
  Csv.defaultOptions { Csv.fieldLabelModifier = dropTypeModifier p }

type GToNamedRecord a
  = (Generic a, Csv.GToRecord (Rep a) (ByteString, ByteString), Typeable a)

instance GToNamedRecord a => Csv.ToNamedRecord (CsvBody a) where
  toNamedRecord =
    Csv.genericToNamedRecord (csvOptions (Proxy :: Proxy a)) . unCsvBody

type GFromNamedRecord a = (Generic a, Csv.GFromNamedRecord (Rep a), Typeable a)

instance GFromNamedRecord a => Csv.FromNamedRecord (CsvBody a) where
  parseNamedRecord x =
    CsvBody <$> Csv.genericParseNamedRecord (csvOptions (Proxy :: Proxy a)) x

type GDefaultOrdered a
  = (Generic a, Csv.GToNamedRecordHeader (Rep a), Typeable a)

instance GDefaultOrdered a => Csv.DefaultOrdered (CsvBody a) where
  headerOrder =
    Csv.genericHeaderOrder (csvOptions (Proxy :: Proxy a)) . unCsvBody

