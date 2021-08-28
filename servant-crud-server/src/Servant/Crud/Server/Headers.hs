{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Servant.Crud.Server.Headers
Description: Servant server implementation of Headers

See also "Servant.Crud.Headers".

-}
module Servant.Crud.Server.Headers
  ( -- * Add Headers
    hLocation
  , hLocation'
  , hLink
  , hLink'
  , hTotalCount
  , hTotalLink'
  -- * Types
  , module Servant.Crud.Headers
  ) where

import           Prelude

import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy                     ( Proxy )
import           Data.Swagger                   ( ToParamSchema(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
                                                ( decodeUtf8 )
import           Network.HTTP.Link              ( Link(..)
                                                , LinkParam(Rel)
                                                )
import           Network.HTTP.Types             ( QueryText
                                                , queryTextToQuery
                                                , renderQuery
                                                )
import           Network.URI                    ( URI(..)
                                                , URIAuth(..)
                                                , parseRelativeReference
                                                )
import           Network.Wai                    ( rawPathInfo )
import           Servant                 hiding ( Link )
import           Servant.Aeson.Internal         ( HasGenericSpecs(..) )
import           Servant.Crud.Server.QueryObject
                                                ( ToParams(..)
                                                , addDocPrefix
                                                )
import           Servant.Docs                   ( DocCapture(..)
                                                , DocQueryParam(..)
                                                , HasDocs(..)
                                                , ParamKind(..)
                                                , ToCapture(..)
                                                , ToSample(..)
                                                , singleSample
                                                )
import           Servant.Ekg                    ( HasEndpoint(..) )
import           Servant.Foreign                ( HasForeign(..)
                                                , HasForeignType(..)
                                                )
import           Servant.QuickCheck.Internal.HasGenRequest
                                                ( HasGenRequest(..) )
import           Servant.Server                 ( HasServer(..) )
import           Servant.Server.Internal.Delayed
                                                ( passToServer )
import           Servant.Swagger.Internal       ( HasSwagger(..) )

import           Servant.Crud.API               ( View'(..) )
import           Servant.Crud.Headers
import           Servant.Crud.QueryObject       ( ToQueryText
                                                , toQueryText
                                                )

instance ToSample PathInfo where
  toSamples _ = singleSample $ PathInfo ["user"]

instance ToParamSchema PathInfo where
  toParamSchema _ = toParamSchema (Proxy :: Proxy [Text])

instance HasServer api ctx => HasServer (PathInfo :> api) ctx where
  type ServerT (PathInfo :> api) m = PathInfo -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route _ ctx d = route (Proxy :: Proxy api) ctx $ passToServer
    d
    ( PathInfo
    . filter (not . Text.null)
    . Text.split (== '/')
    . Text.decodeUtf8
    . rawPathInfo
    )

instance HasDocs api => HasDocs (PathInfo :> api ) where
  docsFor Proxy (endpoint, action) = docsFor subApiP (endpoint, action)
    where subApiP = Proxy :: Proxy api

instance HasSwagger api => HasSwagger (PathInfo :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)

instance HasEndpoint api => HasEndpoint (PathInfo :> api) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy api)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy api)

instance HasGenRequest api
    => HasGenRequest (PathInfo :> api) where
  genRequest _ = genRequest (Proxy :: Proxy api)

instance HasGenericSpecs api => HasGenericSpecs (PathInfo :> api) where
  collectRoundtripSpecs settings Proxy =
    collectRoundtripSpecs settings (Proxy :: Proxy api)

instance HasForeign lang ftype api => HasForeign lang ftype (PathInfo :> api) where
  type Foreign ftype (PathInfo :> api) = Foreign ftype api

  foreignFor lang ftype Proxy = foreignFor lang ftype (Proxy :: Proxy api)

-- | Adds a location header for a route PathInfo and a newly created id
--
-- Useful for post requests
--
-- > type AddBookApi = PathInfo :> (ReqBody '[JSON] Book) :> PostCreated_
-- > type PostCreated_ = PostCreated '[JSON] (Headers '[Header "Location" PathInfo] ())
-- >
-- > postBookServer :: PathInfo -> Book -> Server PostCreated_
-- > postBookServer url book = do
-- >       i <- saveBookAndReturnId book
-- >       pure $ hLocation url i ()
hLocation
  :: (AddHeader "Location" PathInfo orig new, ToHttpApiData id)
  => PathInfo
  -> id
  -> orig
  -> new
hLocation base i = addHeader (PathInfo (runPathInfo base ++ [toUrlPiece i]))

-- | Specialized version of 'hLocation' when @orig@ is @()@.
--
-- > postBookServer :: PathInfo -> Book -> Server PostCreated_
-- > postBookServer url book = hLocation url <$> saveBookAndReturnId book
hLocation'
  :: (AddHeader "Location" PathInfo id new, ToHttpApiData id)
  => PathInfo
  -> id
  -> new
hLocation' base i = hLocation base i i

instance HasForeignType lang ftype Integer => ToParams lang ftype Page where
  toParams t lang ftype _ =
    [(addDocPrefix t limDoc, schema, typ), (addDocPrefix t offDoc, schema, typ)]
   where
    schema = toParamSchema (Proxy :: Proxy Integer)
    typ    = typeFor lang ftype (Proxy :: Proxy Integer) -- TODO Check this (should it be Maybe Integer?)
    limDoc =
      DocQueryParam "limit" [] "Maximum number of results to return" Normal
    offDoc = DocQueryParam "offset" [] "How many rows to skip" Normal


instance ToSample Link where
  toSamples _ =
    [ ( "A link to the next page"
      , Link (example_com "?limit=10&offset=30") [(Rel, "next")]
      )
    , ( "A link to the previous page"
      , Link (example_com "?limit=10&offset=10") [(Rel, "prev")]
      )
    ]
   where
    example_com :: String -> URI
    example_com q =
      URI "http:" (Just (URIAuth "" "www.example.com" "")) "/users" q ""

instance ToParamSchema Link where
  toParamSchema _ = mempty -- TODO

-- | Add Link headers based on the limit, offset, and other query parameters, such as
-- the current filtering and ordering, to the next and previous page.
--
-- If the limit is 'Nothing', then there is no previous and next page, so no headers are added
--
-- If the offet is 'Just 0'  or 'Nothing', then only a link to the next page is added.
hLink
  :: (AddHeader "Link" Link orig new, AddHeader "Link" Link new new2)
  => PathInfo
  -> QueryText
  -> Page
  -> orig
  -> new2
hLink path qs (Page moff mlim) = opt $ do
  lim <- mlim
  let off = fromMaybe 0 moff
  url <- parseRelativeReference . Text.unpack $ toUrlPiece path
  pure $ mkLink lim off url
 where
  mkLink :: Integer -> Integer -> URI -> (Link, Maybe Link)
  mkLink lim off url =
    ( Link (url { uriQuery = mkQuery lim (off + lim) }) [(Rel, "next")]
    , if off > 0
      then Just $ Link (url { uriQuery = mkQuery lim (max (off - lim) 0) })
                       [(Rel, "prev")]
      else Nothing
    )

  mkQuery :: Integer -> Integer -> String
  mkQuery lim off =
    Text.unpack
      .  Text.decodeUtf8
      .  renderQuery True
      .  queryTextToQuery
      $  filter ((`notElem` ["limit", "offset"]) . fst) qs
      ++ offQuery lim off

  offQuery :: Integer -> Integer -> QueryText
  offQuery lim off =
    [ ("limit" , Just $ Text.pack $ show lim)
    , ("offset", Just $ Text.pack $ show off)
    ]

  opt
    :: (AddHeader "Link" Link orig new, AddHeader "Link" Link new new2)
    => Maybe (Link, Maybe Link)
    -> orig
    -> new2
  opt Nothing             = noHeader . noHeader
  opt (Just (x, Nothing)) = addHeader x . noHeader
  opt (Just (x, Just y )) = addHeader x . addHeader y

-- | Add Link headers based on the limit, offset, and the current view parameters, such as
-- the current filtering and ordering, to the next and previous page.
--
-- If the limit is 'Nothing', then there is no previous and next page, so no headers are added
--
-- If the offet is 'Just 0'  or 'Nothing', then only a link to the next page is added.
hLink'
  :: forall orig new new2 c r t
   . ( AddHeader "Link" Link orig new
     , AddHeader "Link" Link new new2
     , ToQueryText (View' c r t)
     )
  => PathInfo
  -> View' c r t
  -> orig
  -> new2
hLink' path view = hLink path (toQueryText "" view) (page view)

-- | Adds a @X-Total-Count@ header.
hTotalCount
  :: forall orig new
   . AddHeader "X-Total-Count" TotalCount orig new
  => Maybe TotalCount
  -> orig
  -> new
hTotalCount (Just i) = addHeader i
hTotalCount Nothing  = noHeader

-- | Adds a @X-Offset@ header.
hOffset
  :: forall orig new
   . AddHeader "X-Offset" Offset orig new
  => Maybe Offset
  -> orig
  -> new
hOffset (Just i) = addHeader i
hOffset _        = noHeader

-- | Link header and total count header
hTotalLink'
  :: forall orig new new2 new3 new4 c r t
   . ( AddHeader "Link" Link orig new
     , AddHeader "Link" Link new new2
     , AddHeader "X-Total-Count" TotalCount new2 new3
     , AddHeader "X-Offset" Offset new3 new4
     , ToQueryText (View' c r t)
     )
  => PathInfo
  -> View' c r t
  -> Maybe TotalCount
  -> orig
  -> new4
hTotalLink' p v c = hOffset o . hTotalCount c . hLink' p v
  where o = Offset <$> offset (page v)

instance ToSample TotalCount where
  toSamples _ =
    [("Suppose there are 42 rows in the requested view.", TotalCount 42)]

instance ToParamSchema TotalCount where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Integer)

instance ToSample Offset where
  toSamples _ =
    [("Suppose there are 20 rows skipped the requested view.", Offset 20)]

instance ToParamSchema Offset where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Integer)

instance ToCapture (Capture "id" id) where
  toCapture _ = DocCapture "id" "The primary key of the resource"
