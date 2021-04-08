{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module: Servant.Crud.Headers
Description: Common headers for Crud applications

-}
module Servant.Crud.Headers
  ( PathInfo(..)
  , Page(..)
  , TotalCount(..)
  )
where

import           Prelude

import           Data.Default                   ( Default(..) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )
import           Servant.API
import           Servant.Aeson.Internal         ( HasGenericSpecs(..) )
import           Servant.Client.Core            ( HasClient(..) )
import           Servant.Foreign                ( HasForeign(..) )

import           Servant.Crud.QueryObject       ( FromQueryText
                                                , ToQueryText
                                                )

-- | Put this type in the route definition if you want the request path info.
--
-- This is mostly useful for generic route handlers, as you would normally know for each
-- handler what route it handles.
--
-- > type Api = PathInfo :> InnerApi
-- >
-- > server :: PathInfo -> Server InnerApi
-- > ...
--
-- The path info contains a list of path segments, i.e. @\/blogs\/foo@ will be
-- @PathInfo { runPathInfo [ "blogs", "foo" ]}@
--
newtype PathInfo = PathInfo { runPathInfo :: [Text] } deriving (Show, Eq)

instance ToHttpApiData PathInfo where
  toUrlPiece (PathInfo xs) = "/" <> Text.intercalate "/" xs

instance FromHttpApiData PathInfo where
  parseUrlPiece =
    pure . PathInfo . filter (not . Text.null) . Text.split (== '/')

instance HasGenericSpecs api => HasGenericSpecs (PathInfo :> api) where
  collectRoundtripSpecs settings Proxy =
    collectRoundtripSpecs settings (Proxy :: Proxy api)

instance HasForeign lang ftype api => HasForeign lang ftype (PathInfo :> api) where
  type Foreign ftype (PathInfo :> api) = Foreign ftype api

  foreignFor lang ftype Proxy = foreignFor lang ftype (Proxy :: Proxy api)

instance HasClient m api => HasClient m (PathInfo :> api) where
  type Client m (PathInfo :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api)

  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

-- | Pageination info
data Page = Page
  { offset :: Maybe Integer -- ^ How many rows to skip, defaulting to zero
  , limit  :: Maybe Integer -- ^ How many rows to return, defaulting to all
  }
  deriving (Generic, Show, Eq)

instance FromQueryText Page
instance ToQueryText Page

instance Default Page where
  def = Page def def

-- | The total number of rows in this view. Nothing indicates an unknown total
newtype TotalCount = TotalCount { unTotalCount :: Integer } deriving (Eq, Show)

instance FromHttpApiData TotalCount where
  parseUrlPiece = fmap TotalCount . parseUrlPiece

instance ToHttpApiData TotalCount where
  toUrlPiece = toUrlPiece . unTotalCount
