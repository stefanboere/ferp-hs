{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Servant.Crud.API
Description: Common verbs

The only content type which is used here is JSON.

-}
module Servant.Crud.API
  ( GetList'
  , Get'
  , Req'
  , ReqCSV'
  -- * Verbs without content
  -- | These all end with '_', indicating no content. The types are opiniated.
  , Post_'
  , Delete_'
  , Put_'
  , Patch_'
  -- * Other
  , View'(..)
  , Page(..)
  , CSV
  , TotalHdr
  , LinkHdr
  , LocationHdr
  , GetListHeaders
  -- * Re-exports
  , Link
  , TotalCount
  , PathInfo
  ) where

import           Prelude

import           Data.Default                   ( Default(..) )
import           Network.HTTP.Link.Types        ( Link )
import           Servant.API             hiding ( Link )
import           Servant.Crud.Headers           ( Page(..)
                                                , PathInfo
                                                , TotalCount
                                                )
import           Servant.Crud.OrderBy           ( OrderBy
                                                , Selectors
                                                )
import           Servant.Crud.QueryObject       ( FromQueryText(..)
                                                , ToQueryText(..)
                                                )

data CSV

type TotalHdr = Header "X-Total-Count" TotalCount

type LinkHdr = Header "Link" Link

type LocationHdr = Header "Location" PathInfo

type GetListHeaders a = Headers '[TotalHdr , LinkHdr , LinkHdr] [a]

-- | 'GET' which returns a JSON array of type @a@ with some extra headers
type GetList' a = Get '[JSON , CSV] (GetListHeaders a)

-- | Get with return type JSON
type Get' = Get '[JSON]

-- | Request body of type JSON
type Req' = ReqBody '[JSON]

-- | Request body of type JSON and CSV
type ReqCSV' = ReqBody '[JSON , CSV]

-- | Empty response with status 201 with the link of the just created resource in the Location header
type Post_' = PostCreated '[JSON] (Headers '[LocationHdr] NoContent)

-- | 'DELETE' status 204
type Delete_' = DeleteNoContent '[JSON] NoContent

-- | 'PUT' status 204
type Put_' = PutNoContent '[JSON] NoContent

-- | 'PATCH' status 204
type Patch_' = PatchNoContent '[JSON] NoContent

-- | All user supplied info about the results they want
data View' c r filterType = View
  { page     :: Page
  , ordering :: [OrderBy c r]
  , filters  :: filterType
  }

instance (Default filterType) => Default (View' c r filterType) where
  def = View def def def

instance (Semigroup filterType) => Semigroup (View' c r filterType) where
  x <> y = View { page     = page x <> page y
                , ordering = ordering x <> ordering y
                , filters  = filters x <> filters y
                }

instance (Monoid filterType) => Monoid (View' c r filterType) where
  mempty = View mempty mempty mempty

instance (Selectors c r, FromQueryText filterType) => FromQueryText (View' c r filterType) where
  fromQueryText t qs =
    View
      <$> fromQueryText t             qs
      <*> fromQueryText (t <> "sort") qs
      <*> fromQueryText t             qs

instance ToQueryText filterType => ToQueryText (View' c r filterType) where
  toQueryTextPrio t (View p o f) =
    toQueryTextPrio t p
      ++ toQueryTextPrio (t <> "sort") o
      ++ toQueryTextPrio t             f
