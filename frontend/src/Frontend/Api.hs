{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend.Api
  ( getBlog
  , putBlog
  , patchBlog
  , deleteBlog
  , deleteBlogs
  , postBlog
  , postBlogs
  , getBlogs
  -- * Utils
  , orAlert
  -- * Re-exports
  , Token(..)
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Data.Proxy
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           GHC.TypeLits                   ( KnownSymbol
                                                , symbolVal
                                                )
import           Reflex.Dom              hiding ( Client
                                                , Link(..)
                                                , rangeInput
                                                )
import qualified Reflex.Dom.Prerender          as Prerender
                                                ( Client )
import           Servant.API             hiding ( URI(..) )
import           Servant.AccessControl          ( Auth'
                                                , Token(..)
                                                )
import           Servant.Common.Req             ( fanReqResult )
import           Servant.Crud.API
import           Servant.Crud.QueryObject       ( QueryObject
                                                , ToQueryText(..)
                                                , toQueryText
                                                )
import           Servant.Reflex

import           Common.Api
import           Common.Schema
import           Components.Alert
import           Components.Class

instance (Reflex t, HasClient t m api tag) => HasClient t m (Auth' auths a v :> api) tag where
  type Client t m (Auth' auths a v :> api) tag
    = Dynamic t Token -> Client t m api tag
  clientWithRouteAndResultHandler Proxy q t req burl opts wrp dynTok =
    clientWithRouteAndResultHandler (Proxy :: Proxy api) q t req' burl opts wrp
   where
    req' = Servant.Reflex.addHeader "Authorization" (Right <$> dynTok) req

instance (HasClient t m api tag) => HasClient t m (PathInfo :> api) tag where
  type Client t m (PathInfo :> api) tag = Client t m api tag
  clientWithRouteAndResultHandler Proxy q t req burl opts wrp =
    clientWithRouteAndResultHandler (Proxy :: Proxy api) q t req burl opts wrp

instance (Reflex t, HasClient t m api tag, ToQueryText a, KnownSymbol sym)
    => HasClient t m (QueryObject sym a :> api) tag where
  type Client t m (QueryObject sym a :> api) tag = a -> Client t m api tag

  clientWithRouteAndResultHandler Proxy q t req burl opts wrp param =
    clientWithRouteAndResultHandler (Proxy :: Proxy api) q t req' burl opts wrp
   where
    req' = req { qParams = ps ++ qParams req }

    ps   = fmap toQueryPart <$> toQueryText paramname param

    toQueryPart Nothing = QueryPartFlag (constDyn True)
    toQueryPart x       = QueryPartParam (constDyn (Right x))

    paramname = Text.pack $ symbolVal (Proxy :: Proxy sym)

instance ToHttpApiData Token where
  toHeader   = ("Bearer " <>) . getToken
  toUrlPiece = Text.decodeUtf8 . toHeader

-- * BLOGS
getBlog
  :: SupportsServantReflex t m
  => Dynamic t Token
  -> Dynamic t (Either Text BlogId)
  -> Event t ()
  -> m (Event t (ReqResult () Blog))
putBlog
  :: SupportsServantReflex t m
  => Dynamic t Token
  -> Dynamic t (Either Text BlogId)
  -> Dynamic t (Either Text Blog)
  -> Event t ()
  -> m (Event t (ReqResult () ()))
patchBlog
  :: SupportsServantReflex t m
  => Dynamic t Token
  -> Dynamic t (Either Text BlogId)
  -> Dynamic t (Either Text BlogPatch)
  -> Event t ()
  -> m (Event t (ReqResult () ()))
deleteBlog
  :: SupportsServantReflex t m
  => Dynamic t Token
  -> Dynamic t (Either Text BlogId)
  -> Event t ()
  -> m (Event t (ReqResult () ()))
deleteBlogs
  :: SupportsServantReflex t m
  => Dynamic t Token
  -> Dynamic t (Either Text [BlogId])
  -> Event t ()
  -> m (Event t (ReqResult () ()))
postBlog
  :: SupportsServantReflex t m
  => Dynamic t Token
  -> Dynamic t (Either Text Blog)
  -> Event t ()
  -> m (Event t (ReqResult () (Headers '[LocationHdr] ())))
postBlogs
  :: SupportsServantReflex t m
  => Dynamic t Token
  -> Dynamic t (Either Text [Blog])
  -> Event t ()
  -> m (Event t (ReqResult () [BlogId]))
getBlogs
  :: SupportsServantReflex t m
  => View Be BlogT
  -> Event t ()
  -> m (Event t (ReqResult () (GetListHeaders Blog)))
getBlog :<|> putBlog :<|> patchBlog :<|> deleteBlog :<|> deleteBlogs :<|> postBlog :<|> postBlogs :<|> getBlogs
  = client clientApi Proxy (Proxy :: Proxy ()) (constDyn url)
  where url = BaseFullUrl Http "localhost" 3005 ""

orAlert
  :: ( Prerender js t m
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Prerender.Client m (Event t (ReqResult tag a))
  -> m (Event t a)
orAlert reqEv = do
  resultEv <- prerender (pure never) reqEv
  let (errEv, rEv) = fanReqResult $ switchDyn resultEv
  alerts def { _alertConfig_status = Danger } errEv
  pure rEv

