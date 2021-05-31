{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Subscriber.Reflex
  ( performWebSocketRequests
  , WebSocketEndpoint
  , FreeClient
  , ClientError(..)
  , ApiWidget
  , runApiWidget
  , requestingJs
  , hoistPure
  , findCookie
  , C.RequestF(..)
  , module Servant.Client.Free
  )
where

import           Control.Exception.Base         ( Exception
                                                , SomeException(..)
                                                )
import           Control.Monad                  ( (>=>) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Free
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.CaseInsensitive          as CI
import           Data.Dependent.Sum             ( DSum(..) )
import           Data.Foldable                  ( toList )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( fromJust )
import qualified Data.Sequence                 as Seq
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           GHCJS.DOM                     as DOM
import           GHCJS.DOM.Document            as DOM
import           GHCJS.DOM.Window              as DOM
import           Language.Javascript.JSaddle    ( MonadJSM )
import qualified Network.HTTP.Types            as C
                                                ( Status(..)
                                                , queryToQueryText
                                                )
import           Network.HTTP.Types.Status      ( status200
                                                , status300
                                                )
import           Network.HTTP.Types.Version     ( http20 )
import           Reflex
import           Reflex.Dom
import qualified Servant.Client.Core           as C
import           Servant.Client.Free     hiding ( Client
                                                , Response
                                                )

import qualified Servant.Subscriber.Compat.Request
                                               as S
import           Servant.Subscriber.Compat.Request
                                                ( HttpRequest(..) )
import qualified Servant.Subscriber.Compat.Response
                                               as S
                                         hiding ( httpHeaders )
import           Servant.Subscriber.Compat.Response
                                                ( HttpResponse(..) )

toSubRequest :: C.Request -> HttpRequest
toSubRequest C.Request {..} = HttpRequest
  { httpMethod  = Text.decodeUtf8 requestMethod
  , httpPath    = S.Path
                  . dropWhile Text.null
                  . Text.splitOn "/"
                  . Text.decodeUtf8
                  . BL.toStrict
                  . B.toLazyByteString
                  $ requestPath
  , httpHeaders = toList $ fmap coerceHdr requestHeaders
  , httpQuery   = C.queryToQueryText $ toList requestQueryString
  , httpBody    = S.RequestBody $ maybe "" (coerceBody . fst) requestBody
  }
 where
  coerceHdr (n, v) = (Text.decodeUtf8 (CI.original n), Text.decodeUtf8 v)
  coerceBody (C.RequestBodyLBS x) = Text.decodeUtf8 . BL.toStrict $ x
  coerceBody (C.RequestBodyBS  x) = Text.decodeUtf8 x
  coerceBody _                    = error "streaming endpoint not handled"


fromSubResponse :: HttpResponse -> C.Response
fromSubResponse HttpResponse {..} = C.Response
  { responseStatusCode  = C.Status
                            (S.statusCode httpStatus)
                            (Text.encodeUtf8 (S.statusMessage httpStatus))
  , responseHeaders     = Seq.fromList
                          $ ("Content-type", "application/json")
                          : map coerceHdr httpHeaders
  , responseHttpVersion = http20
  , responseBody        = BL.fromStrict $ Text.encodeUtf8 httpBody
  }
  where coerceHdr (n, v) = (CI.mk (Text.encodeUtf8 n), Text.encodeUtf8 v)

type FreeClient = Free ClientF
type WebSocketEndpoint = Text

websocketEncoder
  :: Free ClientF a
  -> (HttpRequest, HttpRequest, HttpResponse -> Either C.ClientError a)
websocketEncoder = go . websocketEncoder'
  where go (x, y) = (fromJust x, fromJust x, y)

websocketEncoder'
  :: Free ClientF a
  -> (Maybe HttpRequest, HttpResponse -> Either C.ClientError a)
websocketEncoder' = \case
  Pure n           -> (Nothing, const (pure n))
  Free (Throw err) -> (Nothing, const (Left err))
  Free (RunRequest req k) ->
    ( Just $ toSubRequest req
    , statusClientError req . fromSubResponse >=> hoistPure . k
    )

statusClientError :: C.Request -> C.Response -> Either C.ClientError C.Response
statusClientError req rsp
  | status200 <= responseStatusCode rsp && responseStatusCode rsp < status300
  = pure rsp
  | otherwise
  = let
      req' = req
        { C.requestBody = Nothing
        , C.requestPath = ( BaseUrl Http "" 0 ""
                          , BL.toStrict $ B.toLazyByteString $ C.requestPath req
                          )
        }
    in  Left $ FailureResponse req' rsp

-- | Converts a FreeClient to Either C.ClientError without performing another IO request
--
-- Useful if you know that already one request has been made
--
-- Returns 'Left MultipleRequestsException' if another request needs to be made
hoistPure :: Free ClientF a -> Either C.ClientError a
hoistPure (Pure n) = pure n
hoistPure (Free (Throw e)) = Left e
hoistPure _ = Left (ConnectionError $ SomeException MultipleRequestsException)

data MultipleRequestsException = MultipleRequestsException
  deriving Show

instance Exception MultipleRequestsException

performWebSocketRequests
  :: (Prerender js t m, Applicative m)
  => WebSocketEndpoint
  -> Event t (RequesterData FreeClient)
  -> m (Event t (RequesterData (Either C.ClientError)))
performWebSocketRequests url req =
  fmap switchPromptlyDyn $ prerender (pure never) $ do
    xsrfToken <- findCookie "XSRF-TOKEN"

    rec w <- jsonWebSocket url $ def
          { _webSocketConfig_send = mergeWith
                                      (<>)
                                      [ fmap (toSubReq . addXsrf xsrfToken)
                                      .   Map.elems
                                      <$> reqs
                                      , pure
                                      .   toSubReq
                                      .   addRetry
                                      .   addXsrf xsrfToken
                                      .   fst
                                      <$> retryRsp
                                      ]
          }
        let rsp       = fmapMaybe (>>= parseResp) $ _webSocket_recv w
        let retryRsp' = patchReq' <$> ffilter isRetry rsp
        retryRsp <- delay 1 retryRsp'
        let goodRsp = patchReq' <$> ffilter (not . isRetry) rsp

        (reqs, _inKeys, rsps) <- matchResponsesWithRequests' websocketEncoder
                                                             req
                                                             goodRsp
                                                             modReqs

        let modReqs = never -- TODO unsubscribe

    pure rsps

 where
  statusCode = S.statusCode . httpStatus . snd
  isRetry r =
    statusCode r == 401 && "X-RETRY" `notElem` fmap fst (S.httpHeaders (fst r))

  toSubReq r | httpMethod r == "GET" = S.Subscribe r
             | otherwise             = S.SimpleRequest r

  parseResp :: S.Response -> Maybe (HttpRequest, HttpResponse)
  parseResp (S.Modified rq resp) =
    Just (rq, HttpResponse (S.Status 200 "") [] resp)
  parseResp (S.HttpRequestFailed rq resp) = Just (rq, resp)
  parseResp _                             = Nothing

  patchReq' (rq, rs) = (patchReq rq, rs)
  patchReq rq = rq
    { S.httpHeaders = filter
                        ((`notElem` ["Cookie", "X-XSRF-TOKEN", "X-RETRY"]) . fst
                        )
                        (S.httpHeaders rq)
    }

  addRetry r = r { S.httpHeaders = ("X-RETRY", "") : S.httpHeaders r }

  addXsrf mtok r =
    let addX = maybe id (\t -> (("X-XSRF-TOKEN", t) :)) mtok
    in  r { S.httpHeaders = addX (S.httpHeaders r) }


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

data Decoder rawResponse response = forall a . Decoder (RequesterDataKey a)
                                                       (  rawResponse
                                                       -> response a
                                                       )

-- | Version of 'matchResponsesWithRequests' which keeps listening for new requests
matchResponsesWithRequests'
  :: forall t rawRequest rawResponse request response m key
   . (MonadFix m, MonadHold t m, Reflex t, Ord key)
  => (forall a . request a -> (key, rawRequest, rawResponse -> response a))
  -- ^ Given a request (from 'Requester'), produces the wire format of the
  -- request and a function used to process the associated response
  -> Event t (RequesterData request)
  -- ^ The outgoing requests
  -> Event t (key, rawResponse)
  -- ^ The incoming responses, tagged by an identifying key
  -> Event t key
  -- ^ Unsubscribe events
  -> m
       ( Event t (Map key rawRequest)
       , Event t key
       , Event t (RequesterData response)
       )
  -- ^ A map of outgoing wire-format requests and an event of responses keyed
  -- by the 'RequesterData' key of the associated outgoing request
matchResponsesWithRequests' f send recv unsub = do
  rec
    waitingFor :: Incremental t (PatchMap key (Decoder rawResponse response)) <-
      holdIncremental mempty $ leftmost
        [ fmap fst outgoing
        , (\n -> PatchMap $ Map.singleton n Nothing) <$> unsub
        ]
    let outgoing = processOutgoing send
        incoming = processIncoming waitingFor recv
  return (fmap snd outgoing, fst <$> incoming, snd <$> incoming)
 where
  processOutgoing
    :: Event t (RequesterData request) -- The outgoing request
    -> Event
         t
         (PatchMap key (Decoder rawResponse response), Map key rawRequest)
    -- The new next-available-key, a map of requests expecting responses, and the tagged raw requests
  processOutgoing out = flip pushAlways out $ \dm -> do
    let result = flip map (requesterDataToList dm) $ \(k :=> v) ->
          let (n, rawReq, rspF) = f v in (n, rawReq, Decoder k rspF)
        patchWaitingFor =
          PatchMap $ Map.fromList $ (\(n, _, dec) -> (n, Just dec)) <$> result
        toSend = Map.fromList $ (\(n, rawReq, _) -> (n, rawReq)) <$> result
    return (patchWaitingFor, toSend)

  processIncoming
    :: Incremental t (PatchMap key (Decoder rawResponse response))
    -- A map of outstanding expected responses
    -> Event t (key, rawResponse)
    -- A incoming response paired with its identifying key
    -> Event t (key, RequesterData response)
    -- The decoded response and a patch that clears the outstanding responses queue
  processIncoming waitingFor inc = flip push inc $ \(n, rawRsp) -> do
    wf <- sample $ currentIncremental waitingFor
    case Map.lookup n wf of
      Nothing               -> return Nothing
      Just (Decoder k rspF) -> do
        let rsp = rspF rawRsp
        return $ Just (n, singletonRequesterData k rsp)


type ApiWidget t m = (RequesterT t FreeClient (Either ClientError) m)

runApiWidget
  :: (Prerender js t m, MonadHold t m, MonadFix m)
  => WebSocketEndpoint
  -> ApiWidget t m a
  -> m a
runApiWidget ws start = do
  rec (x, requests) <- runRequesterT start responses
      responses     <- performWebSocketRequests ws requests
  pure x

requestingJs
  :: (MonadFix m, Prerender js t m)
  => Event t (Request (Client (ApiWidget t m)) a)
  -> ApiWidget t m (Event t (Response (Client (ApiWidget t m)) a))
requestingJs r =
  fmap (switch . current) $ prerender (pure never) $ requesting r

