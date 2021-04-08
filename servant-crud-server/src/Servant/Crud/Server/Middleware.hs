{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Servant.Crud.Server.Middleware
Description: Wai middlewares
-}
module Servant.Crud.Server.Middleware
  ( allowHeaderMiddleware
  )
where

import           Prelude

import           Data.List                      ( intersperse
                                                , nub
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Network.HTTP.Types.Header      ( Header
                                                , hAllow
                                                )
import           Network.HTTP.Types.Status      ( methodNotAllowed405 )
import           Network.HTTP.Types.Method      ( Method )
import           Network.Wai                    ( responseStatus
                                                , mapResponseHeaders
                                                , Response
                                                , pathInfo
                                                , Middleware
                                                )
import           Servant.Foreign

-- | Add an allow header if the method is not allowed
allowHeaderMiddleware
  :: ( HasForeign NoTypes NoContent api
     , GenerateList NoContent (Foreign NoContent api)
     )
  => Proxy api
  -> Middleware
allowHeaderMiddleware api = modifyResponse'
  (\pinfo resp -> if responseStatus resp == methodNotAllowed405
    then mapResponseHeaders (methods pinfo :) resp
    else resp
  )
 where
    -- | apply a function that modifies a response as a 'Middleware'
  modifyResponse'
    :: ([Text] -> Network.Wai.Response -> Network.Wai.Response) -> Middleware
  modifyResponse' f appl req respond = appl req $ respond . f (pathInfo req)

  mlist = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) api

  methods :: [Text] -> Network.HTTP.Types.Header.Header
  methods ts = makeHeader $ mapMaybe (getMethod ts) mlist

  getMethod :: [Text] -> Req NoContent -> Maybe Method
  getMethod rs ps | sameLength && matchingSegments = Just (_reqMethod ps)
                  | otherwise                      = Nothing
   where
    pattern'         = _path $ _reqUrl ps
    sameLength       = length rs == length pattern'
    matchingSegments = and $ zipWith matchSegment rs pattern'

    matchSegment :: Text -> Segment NoContent -> Bool
    matchSegment a (Segment (Servant.Foreign.Static (PathSegment b))) | a /= b =
      False
    matchSegment _ _ = True

  makeHeader :: [Method] -> Network.HTTP.Types.Header.Header
  makeHeader ms = (hAllow, mconcat . intersperse ", " $ ("OPTIONS" : nub ms))
