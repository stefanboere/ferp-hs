{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module: Servant.Crud.API
Description: Common verbs

The only content type which is used here is JSON.

-}
module Servant.Crud.API
  ( GetList'
  , Get'
  , Req'
  -- * Verbs without content
  -- | These all end with '_', indicating no content. The types are opiniated.
  , Post_'
  , Delete_'
  , Put_'
  , Patch_'
  -- * Other
  , View'(..)
  , Page(..)
  )
where

import           Prelude

import           Data.Default                   ( Default(..) )
import           Network.HTTP.Link.Types        ( Link )
import           Servant.API             hiding ( Link )
import           Servant.CSV.Cassava
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

-- | 'GET' which returns a JSON array of type @a@ with some extra headers
type GetList' a
  = Get
      '[JSON, CSV]
      ( Headers
          '[Header "X-Total-Count" TotalCount, Header "Link" Link, Header
            "Link"
            Link]
          [a]
      )

-- | Get with return type JSON
type Get' = Get '[JSON]

-- | Request body of type JSON
type Req' = ReqBody '[JSON]

-- | Empty response with status 201 with the link of the just created resource in the Location header
type Post_' = PostCreated '[JSON] (Headers '[Header "Location" PathInfo] ())

-- | 'DELETE' status 204
type Delete_' = DeleteNoContent '[JSON] ()

-- | 'PUT' status 204
type Put_' = PutNoContent '[JSON] ()

-- | 'PATCH' status 204
type Patch_' = PatchNoContent '[JSON] ()

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
    View <$> fromQueryText t qs <*> fromQueryText t qs <*> fromQueryText t qs

instance ToQueryText filterType => ToQueryText (View' c r filterType) where
  toQueryTextPrio t (View p o f) =
    toQueryTextPrio t p ++ toQueryTextPrio t o ++ toQueryTextPrio t f
