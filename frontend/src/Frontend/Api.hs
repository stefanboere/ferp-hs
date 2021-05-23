{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
  , requestBtn
  , withXsrfHeader
  , usingCookie
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHCJS.DOM                     as DOM
import           GHCJS.DOM.Document            as DOM
import           GHCJS.DOM.Window              as DOM
import           Language.Javascript.JSaddle    ( MonadJSM )
import           Reflex.Dom              hiding ( Client
                                                , Link(..)
                                                , rangeInput
                                                )
import qualified Reflex.Dom.Prerender          as Prerender
                                                ( Client )
import           Servant.API             hiding ( URI(..) )
import           Servant.AccessControl          ( Token(..) )
import           Servant.Crud.API
import qualified Servant.Subscriber.Reflex     as Sub
import           Servant.Subscriber.Reflex      ( ApiWidget
                                                , FreeClient
                                                )

import           Common.Api
import           Common.Schema
import           Components.Alert
import           Components.Button
import           Components.Class

orAlert
  :: ( Prerender js t m
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Show err
     )
  => Event t (Either err a)
  -> m (Event t a)
orAlert resultEv = do
  let (errEv, rEv) = fanEither resultEv
  alerts def { _alertConfig_status = Danger } (Text.pack . show <$> errEv)
  pure rEv

requestBtn
  :: ( DomBuilder t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => (Dynamic t ActionState -> ApiWidget t m (Event t ()))
  -> (  Event t ()
     -> Event t (Request (Prerender.Client (ApiWidget t m)) a)
     )
  -> Dynamic t Bool
  -> Event t ()
  -> ApiWidget
       t
       m
       (Event t (Response (Prerender.Client (ApiWidget t m)) a))
requestBtn mkBtn req dynDisabled reqEvAuto = do

  rec dynState <- holdDyn def $ leftmost
        [ ActionLoading <$ reqEv
        , responseState <$> resultEv
        , def <$ resultEvDelay
        ]

      reqEvMan <- mkBtn ((disabledState <$> dynDisabled) <> dynState)
      let reqEv = leftmost [reqEvAuto, reqEvMan]

      resultEv      <- Sub.requestingJs (req reqEv)

      resultEvDelay <- debounce 2 resultEv

  pure resultEv
 where
  disabledState True  = ActionDisabled
  disabledState False = ActionAvailable

  responseState (Right _) = ActionSuccess
  responseState _         = ActionError

-- | Supplying a token is not needed for xhr because the cookie is sent.
-- This is a utility for this.
usingCookie :: Token
usingCookie = Token ""

withXsrfHeader :: MonadJSM m => XhrRequest a -> m (XhrRequest a)
withXsrfHeader r@(XhrRequest _ _ cfg) = do
  cookie <- findCookie "XSRF-TOKEN"
  let addXsrf = maybe id (Map.insert "X-XSRF-TOKEN") cookie
  let c' = cfg
        { _xhrRequestConfig_headers = addXsrf (_xhrRequestConfig_headers cfg)
        , _xhrRequestConfig_withCredentials = True
        }
  pure $ r { _xhrRequest_config = c' }

findCookie :: MonadJSM m => Text -> m (Maybe Text)
findCookie x = do
  window  <- DOM.currentWindowUnchecked
  doc     <- DOM.getDocument window
  cookies <- DOM.getCookie doc
  let cs =
        fmap (fmap (Text.dropWhile (== '=')) . Text.span (/= '=') . Text.strip)
          . Text.splitOn ";"
          $ cookies

  pure (lookup x cs)

getBlog :: Token -> BlogId -> FreeClient Blog
putBlog :: Token -> BlogId -> Blog -> FreeClient NoContent
patchBlog :: Token -> BlogId -> BlogPatch -> FreeClient NoContent
deleteBlog :: Token -> BlogId -> FreeClient NoContent
deleteBlogs :: Token -> [BlogId] -> FreeClient NoContent
postBlog :: Token -> Blog -> FreeClient (Headers '[LocationHdr] NoContent)
postBlogs :: Token -> [Blog] -> FreeClient [BlogId]
getBlogs :: View Be BlogT -> FreeClient (GetListHeaders Blog)
getBlog :<|> putBlog :<|> patchBlog :<|> deleteBlog :<|> deleteBlogs :<|> postBlog :<|> postBlogs :<|> getBlogs
  = Sub.client clientApi
