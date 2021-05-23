{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Client.Reflex
  ( performXhrRequests
  , ApiEndpoint
  , ClientError(..)
  , ApiWidget
  , runApiWidgetXhr
  , requestingJs
  , module Servant.Client.Free
  )
where

import           Control.Concurrent             ( newEmptyMVar
                                                , putMVar
                                                , takeMVar
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Free
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.CaseInsensitive          as CI
import           Data.Foldable                  ( toList )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as Text
import           Language.Javascript.JSaddle    ( MonadJSM )
import qualified Network.HTTP.Types            as C
                                                ( Status(..)
                                                , renderQuery
                                                )
import           Network.HTTP.Types.Version     ( http20 )
import           Reflex
import           Reflex.Dom
import qualified Servant.Client.Core           as C
import           Servant.Client.Free     hiding ( Client
                                                , Response
                                                )

import           Servant.Subscriber.Reflex      ( ApiWidget
                                                , hoistPure
                                                , requestingJs
                                                )

toXhrRequest :: Text -> C.Request -> XhrRequest Text
toXhrRequest baseUrl C.Request {..} = XhrRequest
  { _xhrRequest_method = Text.decodeUtf8 requestMethod
  , _xhrRequest_url = let path = BL.toStrict . B.toLazyByteString $ requestPath
                          query =
                            C.renderQuery True $ toList requestQueryString
                      in  baseUrl <> Text.decodeUtf8 (path <> query)
  , _xhrRequest_config = (def :: XhrRequestConfig ())
    { _xhrRequestConfig_headers = Map.fromList
                                  $ (("Content-Type", "application/json") :)
                                  $ toList
                                  $ fmap coerceHdr requestHeaders
    , _xhrRequestConfig_sendData = maybe "" (coerceBody . fst) requestBody
    , _xhrRequestConfig_responseHeaders = OnlyHeaders
                                            (Set.singleton "Content-type")
    }
  }
 where
  coerceHdr (n, v) = (Text.decodeUtf8 (CI.original n), Text.decodeUtf8 v)
  coerceBody (C.RequestBodyLBS x) = Text.decodeUtf8 . BL.toStrict $ x
  coerceBody (C.RequestBodyBS  x) = Text.decodeUtf8 x
  coerceBody _                    = error "streaming endpoint not handled"


fromXhrResponse :: XhrResponse -> C.Response
fromXhrResponse XhrResponse {..} = C.Response
  { responseStatusCode  = C.Status (fromIntegral _xhrResponse_status)
                                   (Text.encodeUtf8 _xhrResponse_statusText)
  , responseHeaders     = Seq.fromList $ map coerceHdr $ Map.toList
                            _xhrResponse_headers
  , responseHttpVersion = http20
  , responseBody        = BL.fromStrict
                            $ Text.encodeUtf8 (fromMaybe "" _xhrResponse_responseText)
  }
  where coerceHdr (n, v) = (CI.map Text.encodeUtf8 n, Text.encodeUtf8 v)

type ApiEndpoint = Text

performXhrRequests
  :: (Prerender js t m, Applicative m)
  => ApiEndpoint
  -> (forall a . XhrRequest a -> Performable (Client m) (XhrRequest a))
  -> Event t (RequesterData (Free ClientF))
  -> m (Event t (RequesterData (Either C.ClientError)))
performXhrRequests apiUrl patchReq req =
  fmap switchPromptlyDyn
    $ prerender (pure never)
    $ performEventAsync
    $ ffor req
    $ \r yield -> liftIO . yield =<< apiRequestXhr apiUrl patchReq r

apiRequestXhr
  :: forall m
   . (MonadIO m, HasJSContext m, MonadJSM m)
  => ApiEndpoint
  -> (forall a . XhrRequest a -> m (XhrRequest a))
  -> RequesterData (Free ClientF)
  -> m (RequesterData (Either C.ClientError))
apiRequestXhr apiUrl patchReq = traverseRequesterData mkRequest
 where
  mkRequest :: Free ClientF b -> m (Either C.ClientError b)
  mkRequest = \case
    Pure n                  -> pure (pure n)
    Free (Throw err       ) -> pure (Left err)
    Free (RunRequest req k) -> do
      response <- liftIO newEmptyMVar
      req'     <- patchReq $ toXhrRequest apiUrl req
      _        <- newXMLHttpRequest req' $ liftIO . putMVar response
      xhrResp  <- liftIO $ takeMVar response
      pure $ hoistPure . k . fromXhrResponse $ xhrResp

runApiWidgetXhr
  :: (Prerender js t m, MonadHold t m, MonadFix m)
  => ApiEndpoint
  -> (forall b . XhrRequest b -> Performable (Client m) (XhrRequest b))
  -> ApiWidget t m a
  -> m a
runApiWidgetXhr apiUrl patchReq start = do
  rec (x, requests) <- runRequesterT start responses
      responses     <- performXhrRequests apiUrl patchReq requests
  pure x

