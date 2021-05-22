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
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                )
import           Network.Wai                    ( Application
                                                , pathInfo
                                                )
import           Network.Wai.Handler.WebSockets ( websocketsOr )
import           Network.WebSockets.Connection  ( acceptRequest
                                                , connectionOnPong
                                                , defaultConnectionOptions
                                                , forkPingThread
                                                )
import           Servant.Subscriber
import qualified Servant.Subscriber.Client     as Client
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
