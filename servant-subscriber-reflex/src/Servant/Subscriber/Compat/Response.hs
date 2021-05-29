{-# LANGUAGE DeriveGeneric #-}
module Servant.Subscriber.Compat.Response
  ( Response(..)
  , HttpResponse(..)
  , Status(..)
  )
where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics

import qualified Servant.Subscriber.Compat.Request
                                               as R

type ResponseHeaders = R.RequestHeaders

-- | Any message from the server is a Response.
--
--   'Subscribed': Resource was successfully subscribed
--
--   'Modified': Resource was modified (this message is also triggered immediately after a subscription)
--
--   'HttpRequestFailed': The server replied with some none 2xx status code.
--   Thus your subscription failed or got removed.
--
--   'ParseError': Your request could not be parsed.

data Response =
    Subscribed !R.HttpRequest
  | Modified !R.HttpRequest !ResponseBody -- If the full response is needed an additional FullSubscribe command with an appropriate additional response type will need to be added.
  | Deleted !R.Path
  | Unsubscribed !R.HttpRequest
  | HttpRequestFailed !R.HttpRequest !HttpResponse
  | ParseError
  deriving Generic

instance ToJSON Response
instance FromJSON Response

data HttpResponse = HttpResponse
  { httpStatus  :: !Status
  , httpHeaders :: !ResponseHeaders
  , httpBody    :: !ResponseBody
  }
  deriving Generic

instance ToJSON HttpResponse
instance FromJSON HttpResponse

data Status = Status
  { statusCode    :: !Int
  , statusMessage :: !Text
  }
  deriving (Generic, Show)

instance ToJSON Status
instance FromJSON Status

type ResponseBody = Text


