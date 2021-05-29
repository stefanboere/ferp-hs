{-# LANGUAGE OverloadedStrings #-}
module WebSocket
  ( serveSubscriber'
  , createSubscriber
  )
where

import           Backend.Logger                 ( TimedFastLogger
                                                , logWithConfig
                                                )
import           Control.Concurrent.STM         ( atomically )
import           Control.Monad                  ( (<=<)
                                                , join
                                                )
import           Control.Monad.Logger           ( runLoggingT )
import qualified Data.CaseInsensitive          as CI
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                )
import qualified Data.Text.Encoding            as Text
import           Network.HTTP.Types.Header      ( hCookie )
import           Network.Wai                    ( Application
                                                , Request
                                                , pathInfo
                                                , requestHeaders
                                                )
import           Network.Wai.Handler.WebSockets ( websocketsOr )
import           Network.WebSockets.Connection  ( acceptRequest
                                                , connectionOnPong
                                                , defaultConnectionOptions
                                                , forkPingThread
                                                )
import           Servant.Subscriber
import qualified Servant.Subscriber.Client     as Client
import qualified Servant.Subscriber.Request    as S
import           Servant.Subscriber.Types       ( Path(..)
                                                , entryPoint
                                                , runLogging
                                                )

import           Context

createSubscriber :: AppInfo -> TimedFastLogger -> IO (Subscriber api)
createSubscriber appInfo logger = atomically
  (makeSubscriber "subscriber" (`runLoggingT` logWithConfig logger appInfo))


serveSubscriber' :: Subscriber api -> Application -> Application
serveSubscriber' subscriber app' req sendResponse = do
  (pongHandler, myRef) <- makePongAction
  let opts = defaultConnectionOptions { connectionOnPong = pongHandler }
  let runLog = runLogging subscriber
  let handleWSConnection pending = do
        connection <- acceptRequest pending
        forkPingThread connection 28
        runLog
          .   Client.run app'
          <=< atomically
          .   fmap (addCookies req)
          .   Client.fromWebSocket subscriber myRef
          $   connection
  if Path (pathInfo req) == entryPoint subscriber
    then websocketsOr opts handleWSConnection app' req sendResponse
    else app' req sendResponse
 where
  makePongAction :: IO (IO (), IORef (IO ()))
  makePongAction = do
    myRef <- newIORef $ return ()
    let action = join $ readIORef myRef
    return (action, myRef)

  addCookies :: Request -> Client.Client api -> Client.Client api
  addCookies r c = c
    { Client.readRequest = fmap (fmapReq (addCookie r)) <$> Client.readRequest c
    }

  addCookie r httpReq = httpReq
    { S.httpHeaders = S.httpHeaders httpReq <> cookieHeaders (requestHeaders r)
    }

  cookieHeaders = map coerceHeader . filter ((== hCookie) . fst)

  coerceHeader (x, y) = (Text.decodeUtf8 (CI.original x), Text.decodeUtf8 y)

  fmapReq f (S.Subscribe       httpReq) = S.Subscribe (f httpReq)
  fmapReq f (S.Unsubscribe     httpReq) = S.Unsubscribe (f httpReq)
  fmapReq f (S.SetPongRequest  httpReq) = S.SetPongRequest (f httpReq)
  fmapReq f (S.SetCloseRequest httpReq) = S.SetCloseRequest (f httpReq)
  fmapReq f (S.SimpleRequest   httpReq) = S.SimpleRequest (f httpReq)

