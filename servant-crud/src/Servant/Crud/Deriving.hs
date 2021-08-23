{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module: Servant.Crud.Deriving
Description: Utilities to automatically derive all kinds of type classes with DerivingVia

This module provides newtypes which can be used to derive instances with the -XDerivingVia
language extension.
-}
module Servant.Crud.Deriving
  ( -- * Newtypes
    JsonBody(..)
  , GToJSON
  , GFromJSON
  , GEq
  , QueryType(..)
    -- * For creating options
  , aesonOptions
  , typeName
  , queryOptions
  , dropTypeModifier
  ) where

import           Prelude

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import qualified Data.Aeson                    as Aeson
import qualified Data.Char                     as C
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Typeable                  ( Typeable
                                                , typeRep
                                                )
import           GHC.Generics
import           Generics.Deriving              ( ConNames )
import           Generics.Generic.Aeson         ( GfromJson
                                                , GtoJson
                                                , Settings(..)
                                                , gparseJsonWithSettings
                                                , gtoJsonWithSettings
                                                )
import           Generics.Generic.IsEnum        ( GIsEnum )
import           Servant.Crud.QueryObject       ( fromAesonOptions )
import           Servant.Crud.QueryObject      as QueryObject


-- | Opiniated options for the ToJSON and FromJSON instances. Strips the prefix and
-- makes the first letter lower case
aesonOptions :: Typeable a => Proxy a -> Aeson.Options
aesonOptions p = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = dropTypeModifier p
  , Aeson.omitNothingFields  = True
  }

dropTypeModifier :: Typeable a => Proxy a -> String -> String
dropTypeModifier p s =
  fromMaybe s (L.stripPrefix (Text.unpack ("_" <> typeName p)) s)

-- | Create a QueryObject options from type based on the prefix (typename)
queryOptions :: Typeable a => Proxy a -> Options
queryOptions = fromAesonOptions . aesonOptions

-- | Converts a type to the used field prefix. The rule is, take the first word,
-- lowercase the first letter, and drop all capital letters from the end.
typeName :: Typeable a => Proxy a -> Text
typeName =
  Text.dropWhileEnd C.isUpper
    . Text.pack
    . lowerHead
    . L.takeWhile (/= ' ')
    . show
    . typeRep
 where
  lowerHead :: String -> String
  lowerHead (x : xs) = C.toLower x : xs
  lowerHead []       = []

-- | Create a default instance for 'Settings' with the prefix dropped
aesonSettings :: Typeable a => Proxy a -> Settings
aesonSettings = Settings . Just . Text.unpack . ("_" <>) . typeName

-- | Newtype for usage with -XDeriveVia to derive instances used for types which are
-- used in request bodies.
--
-- > deriving via (JsonBody $(t)) instance ToJSON $(t)
newtype JsonBody a = JsonBody { unJsonBody :: a }

type GToJSON a
  = (Generic a, GtoJson (Rep a), ConNames (Rep a), GIsEnum (Rep a), Typeable a)

-- | A common type name prefix is dropped
instance GToJSON a => ToJSON (JsonBody a) where
  toJSON = gtoJsonWithSettings (aesonSettings (Proxy :: Proxy a)) . unJsonBody

type GFromJSON a
  = ( Generic a
    , GfromJson (Rep a)
    , ConNames (Rep a)
    , GIsEnum (Rep a)
    , Typeable a
    )

-- | A common type name prefix is dropped
instance GFromJSON a
    => FromJSON (JsonBody a) where
  parseJSON x =
    JsonBody <$> gparseJsonWithSettings (aesonSettings (Proxy :: Proxy a)) x


-- | Newtype for usage with -XDeriveVia to derive instances for types which are
-- used in query parameters
--
-- > deriving via QueryType $(t) instance FromQueryText $(t)
newtype QueryType a = QueryType { unQueryType :: a }

-- | A common type name prefix is dropped
instance (Generic a, Typeable a, GFromQueryText (Rep a)) => FromQueryText (QueryType a) where
  fromQueryText t xs =
    QueryType <$> defaultFromQueryText (queryOptions (Proxy :: Proxy a)) t xs

-- | A common type name prefix is dropped
instance (Generic a, Typeable a, GToQueryText (Rep a)) => ToQueryText (QueryType a) where
  toQueryTextPrio t =
    defaultToQueryText (queryOptions (Proxy :: Proxy a)) t . unQueryType

type GEq t = Eq (Rep t ())
