{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Semigroup                 ( Min(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Link              ( Link(..)
                                                , parseLinkHeader'
                                                , parseLinkHeaderBS'
                                                )
import           Servant.API             hiding ( Link )
import           Servant.Client.Core            ( HasClient(..) )

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

-- | Calculates the intersection between two pages, then it satisfies the monoid laws with def (the entire set)
instance Semigroup Page where
  x <> y =
    let offset_x  = fromMaybe 0 (offset x)
        offset_y  = fromMaybe 0 (offset y)
        offset_xy = max offset_x offset_y
        last_x    = Min . (+ offset_x) <$> limit x
        last_y    = Min . (+ offset_y) <$> limit y
        last_xy   = (\s -> s - offset_xy) . getMin <$> (last_x <> last_y)
    in  Page { offset = if offset_xy == 0 then Nothing else Just offset_xy
             , limit  = max 0 <$> last_xy
             }

instance Monoid Page where
  mempty = def

-- | The total number of rows in this view. Nothing indicates an unknown total
newtype TotalCount = TotalCount { unTotalCount :: Integer } deriving (Eq, Show)

instance FromHttpApiData TotalCount where
  parseUrlPiece = fmap TotalCount . parseUrlPiece

instance ToHttpApiData TotalCount where
  toUrlPiece = toUrlPiece . unTotalCount

instance FromHttpApiData [Link] where
  parseUrlPiece = either (Left . Text.pack) Right . parseLinkHeader'
  parseHeader   = either (Left . Text.pack) Right . parseLinkHeaderBS'

instance FromHttpApiData Link where
  parseUrlPiece r =
    parseUrlPiece r >>= maybe (Left "No link found") Right . listToMaybe
  parseHeader r =
    parseHeader r >>= maybe (Left "No link found") Right . listToMaybe
