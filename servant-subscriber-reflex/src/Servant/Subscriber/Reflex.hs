{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
  ) where

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
import           Data.Function                  ( fix )
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
import           Unsafe.Coerce                  ( unsafeCoerce )

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax
import           Unsafe.TrueName

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
  -> m (Event t (RequesterData (Either C.ClientError)), Event t ())
performWebSocketRequests url req =
  fmap flatten $ prerender (pure (never, never)) $ do
    xsrfToken <- findCookie "XSRF-TOKEN"

    rec
      w <- jsonWebSocket url $ def
        { _webSocketConfig_send = mergeWith
          (<>)
          [ fmap (toSubReq . addXsrf xsrfToken) . Map.elems <$> reqs
          , pure . toSubReq . addRetry . addXsrf xsrfToken . fst <$> retryRsp
          , pure . S.Unsubscribe . addXsrf xsrfToken <$> modReqs
          ]
        }
      let rsp       = fmapMaybe (>>= parseResp) $ _webSocket_recv w
      let retryRsp' = patchReq' <$> ffilter isRetry rsp
      retryRsp <- delay 1 retryRsp'
      let goodRsp = patchReq' <$> ffilter (not . isRetry) rsp

      (reqs, inKeys, rsps) <- matchResponsesWithRequests' websocketEncoder
                                                          req
                                                          goodRsp

      reqKeys <- foldDyn patchReqKeys Map.empty modReqs'
      let modReqs' = attachWithMaybe toUnsub (current reqKeys) inKeys
      let modReqs  = fmapMaybe (\(_, _, x) -> x) modReqs'

    pure (rsps, _webSocket_error w)

 where
  flatten x = ( switchPromptlyDyn (fst <$> x), switchPromptlyDyn (snd <$> x))
  -- If a request from the same orig is done again, the old one is unsubscribed
  -- This allows for e.g. dynamic filtering,
  -- i.e. automatic unsubscribing for the original filter if the filter changes
  toUnsub
    :: Map [[Int]] HttpRequest
    -> ([[Int]], HttpRequest)
    -> Maybe ([[Int]], HttpRequest, Maybe HttpRequest)
  toUnsub prev (k, r)
    | httpMethod r == "GET"
    = let r'  = prev Map.!? k
          r'' = if Just r == r' then Nothing else r'
      in  Just (k, r, r'')
    | otherwise
    = Nothing

  patchReqKeys (k, r, _) m = Map.insert k r m

  statusCode = S.statusCode . httpStatus . snd
  isRetry r = statusCode r == 401 && "X-RETRY" `notElem` fmap
    fst
    (S.httpHeaders (fst r))

  toSubReq r | httpMethod r == "GET" = S.Subscribe r
             | otherwise             = S.SimpleRequest r

  parseResp :: S.Response -> Maybe (HttpRequest, HttpResponse)
  parseResp (S.Modified          rq resp) = Just (rq, resp)
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
  -> m
       ( Event t (Map key rawRequest)
       , Event t ([[Int]], key)
       , Event t (RequesterData response)
       )
  -- ^ A map of outgoing wire-format requests and an event of responses keyed
  -- by the 'RequesterData' key of the associated outgoing request
matchResponsesWithRequests' f send recv = do
  rec
    waitingFor :: Incremental t (PatchMap key (Decoder rawResponse response)) <-
      holdIncremental mempty $ fmap fst outgoing

    let outgoing = processOutgoing send
        incoming = processIncoming waitingFor recv
  return (fmap snd outgoing, keyWithOrig <$> incoming, snd <$> incoming)
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


  keyWithOrig :: (key, RequesterData response) -> ([[Int]], key)
  keyWithOrig (x, y) = (requesterDataHash y, x)


-- | Calculate a unique number based on the origin of the request.
-- This number is used to automatically unsubscribe if caller subscribes to a different request.
-- That is, stop being subscribed to the old filter if the filter updates
requesterDataHash :: RequesterData response -> [[Int]]
requesterDataHash = fmap toHashSingle . requesterDataToList
  where
    toHashSingle (x :=> _) = fix go x

    -- Extremely dirty implementation by bringing nonexported constructors into
    -- scope using Template Haskell and furthermore some unsafeCoerce here and there
    go = $(lamE [varP (mkName "rec")] $ lamCaseE
      [ match
          ((`ConP`
            [VarP (mkName "x")]
           )
              <$> summon "RequesterDataKey_Single" ''RequesterDataKey
          )
          (normalB (listE
            [ appE (varE (mkName "unsafeCoerce")) (varE (mkName "x"))
            ])
          )
          []
      , match
          ((`ConP`
            [VarP (mkName "x"), VarP (mkName "y"), VarP (mkName "z")]
           )
              <$> summon "RequesterDataKey_Multi" ''RequesterDataKey
          )
          (normalB
            (infixE (Just $ listE
              [ appE (varE (mkName "unsafeCoerce")) (varE (mkName "x"))
              , varE (mkName "y")
              ])
              (varE (mkName "++"))
              (Just $ appE (varE (mkName "rec")) (varE (mkName "z")))
            )
          )
          []
      , match
          ((`ConP`
            [ VarP (mkName "x")
            , WildP
            , VarP (mkName "y")
            , VarP (mkName "z")
            ]
           )
              <$> summon "RequesterDataKey_Multi2" ''RequesterDataKey
          )
          (normalB
            (infixE (Just $ listE
              [ appE (varE (mkName "unsafeCoerce")) (varE (mkName "x"))
              , varE (mkName "y")
              ])
              (varE (mkName "++"))
              (Just $ appE (varE (mkName "rec")) (varE (mkName "z")))
            )
          )
          []
      , match
          ((`ConP`
            [ VarP (mkName "w")
            , VarP (mkName "x")
            , VarP (mkName "y")
            , VarP (mkName "z")
            ]
           )
              <$> summon "RequesterDataKey_Multi3" ''RequesterDataKey
          )
          (normalB
            (infixE (Just $ listE
              [ appE (varE (mkName "unsafeCoerce")) (varE (mkName "w"))
              , varE (mkName "x")
              , varE (mkName "y")
              ])
              (varE (mkName "++"))
              (Just $ appE (varE (mkName "rec")) (varE (mkName "z")))
            )
          )
          []
      ]
      )

type ApiWidget t m = (RequesterT t FreeClient (Either ClientError) m)

runApiWidget
  :: (Prerender js t m, MonadHold t m, MonadFix m)
  => WebSocketEndpoint
  -> ApiWidget t m a
  -> m (a, Event t ())
runApiWidget ws start = do
  rec (x, requests) <- runRequesterT start responses
      (responses, errs)     <- performWebSocketRequests ws requests
  pure (x, errs)

requestingJs
  :: (Requester t (Client m), Applicative m, Prerender js t m)
  => Event t (Request (Client m) a)
  -> m (Event t (Response (Client m) a))
requestingJs r =
  fmap (switch . current) $ prerender (pure never) $ requesting r

