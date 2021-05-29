{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Servant.Subscriber.Compat.Request
  ( HttpRequest(..)
  , Request(..)
  , RequestBody(..)
  , RequestHeader
  , RequestHeaders
  , Path(..)
  )
where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics
import qualified Network.HTTP.Types            as H


newtype Path = Path [Text] deriving (Eq, Generic, Ord, Show, ToJSON, FromJSON)


--   We don't use Network.HTTP.Types here, because we need FromJSON instances, which can not
--   be derived for 'ByteString'
type RequestHeader = (Text, Text)
type RequestHeaders = [RequestHeader]


-- | Any message from the client is a 'Request':
--
--   `SetPongRequest`: A request that should be issued whenever a websocket pong is received.
--   In addition to every websocket pong the request also gets issued
--   immediately upon receival. Bot `SetPongRequest` and `SetCloseRequest` will
--   be confirmed with a `Subscribed` response, but any return value of the
--   request won't be delivered.
--
--   `SetCloseRequest`: A request that should be issued when the websocket
--   connection closes for whatever reason.
data Request = Subscribe !HttpRequest
             | Unsubscribe !HttpRequest
             | SetPongRequest !HttpRequest
             | SetCloseRequest !HttpRequest
             | SimpleRequest !HttpRequest deriving (Generic)

instance FromJSON Request
instance ToJSON Request


data HttpRequest = HttpRequest
  { httpMethod  :: !Text
  , httpPath    :: !Path
  , httpHeaders :: RequestHeaders
  , httpQuery   :: H.QueryText
  , httpBody    :: RequestBody
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON HttpRequest
instance ToJSON HttpRequest

newtype RequestBody = RequestBody Text deriving (Generic, ToJSON, FromJSON, Eq, Ord, Show)
