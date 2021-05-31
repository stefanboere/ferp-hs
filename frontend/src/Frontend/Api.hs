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
  , orAlertF
  , requestBtn
  , runApi
  , withXsrfHeader
  , usingCookie
  , refreshAccessTokenEvery
  )
where

import           Control.Exception.Base         ( displayException )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Time                      ( NominalDiffTime )
import           Language.Javascript.JSaddle    ( MonadJSM )
import           Network.HTTP.Types
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
                                                , ClientError(..)
                                                )

import           Common.Api
import           Common.Schema
import           Components.Alert
import           Components.Button
import           Components.Class

orAlertF
  :: ( Prerender js t m
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => m (Event t (Either ClientError a))
  -> m (Event t a)
orAlertF getResultEv = do
  resultEv <- getResultEv
  let (errEv, rEv) = fanEither resultEv
  alerts def { _alertConfig_status = Danger } (showError <$> errEv)
  pure rEv

showError :: (Applicative m, Prerender js t m) => ClientError -> (Text, m ())
showError (FailureResponse rq rsp) =
  let status  = Sub.responseStatusCode rsp
      statusI = statusCode status
      msg =
          Text.unlines
            . catMaybes
            $ [ Just
              $  "The request to "
              <> showUrl (Sub.requestPath rq)
              <> " failed. The server responded with "
              <> (Text.pack . show $ statusI)
              <> " "
              <> (Text.decodeUtf8 . statusMessage $ status)
              <> "."
              , if BL.null (Sub.responseBody rsp)
                then Nothing
                else Just $ Text.decodeUtf8 (BL.toStrict $ Sub.responseBody rsp)
              , if statusI == 403
                then Just "Please try reloading this page."
                else Nothing
              ]
  in  (msg, if statusI == 403 then reloadAction else pure ())

showError (DecodeFailure x _) =
  ("The response decoding failed: " <> x, pure ())
showError (UnsupportedContentType x _) =
  ("The content type " <> Text.pack (show x) <> " is not supported.", pure ())
showError (InvalidContentTypeHeader _) =
  ("The content type header of the response is invalid.", pure ())
showError (ConnectionError e) =
  ("A connection error occured: " <> Text.pack (displayException e), pure ())

showUrl :: (Sub.BaseUrl, ByteString) -> Text
showUrl (_, p) = Text.decodeUtf8 p

reloadAction :: (Applicative m, Prerender js t m) => m ()
reloadAction = prerender_ (pure ()) $ do
  loc <- getLocationAfterHost
  elAttr "a" ("href" =: loc) (text "Reload page")

orAlert
  :: ( Prerender js t m
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t (Either ClientError a)
  -> m (Event t a)
orAlert resultEv = orAlertF (pure resultEv)

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
  cookie <- Sub.findCookie "XSRF-TOKEN"
  let addXsrf = maybe id (Map.insert "X-XSRF-TOKEN") cookie
  let c' = cfg
        { _xhrRequestConfig_headers = addXsrf (_xhrRequestConfig_headers cfg)
        , _xhrRequestConfig_withCredentials = True
        }
  pure $ r { _xhrRequest_config = c' }

refreshAccessTokenEvery
  :: (Applicative m, Prerender js t m) => NominalDiffTime -> m ()
refreshAccessTokenEvery interval = prerender_ (pure ()) $ do
  tickEv <- tickLossyFromPostBuildTime interval
  refreshAccessTokenXhr (() <$ tickEv)

refreshAccessTokenXhr
  :: ( MonadIO m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , HasJSContext (Performable m)
     , TriggerEvent t m
     )
  => Event t ()
  -> m ()
refreshAccessTokenXhr ev = ignore <$> getAndDecode ("/auth/refresh" <$ ev)
 where
  ignore :: Event t (Maybe ()) -> ()
  ignore _ = ()

runApi
  :: (MonadHold t m, MonadFix m, Prerender js t m) => ApiWidget t m a -> m a
runApi = Sub.runApiWidget "ws://localhost:3005/subscriber"

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
