{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Context
  ( Config(..)
  , AppConfig(..)
  , defaultAppConfig
  , AuthUser(..)
  , AppT
  , GlobalAlert(..)
  , getConfigFromPage
  , getConfigFromFile
  , runAppT
  , showClientError
  , FutureMaybe(..)
  , getUserNow
  )
where

import           Control.Exception.Base         ( displayException )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader           ( ReaderT(..) )
import           Data.Aeson
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import           Data.Maybe                     ( catMaybes )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           GHC.Generics                   ( Generic )
import           GHCJS.DOM
import           GHCJS.DOM.Document             ( getHead )
import           GHCJS.DOM.Element              ( getInnerHTML )
import           GHCJS.DOM.ParentNode           ( querySelector )
import           Network.HTTP.Types
import           Reflex.Dom
import           Language.Javascript.JSaddle

import           Servant.AccessControl          ( Token(..) )
import qualified Servant.Subscriber.Reflex     as Sub
import           Servant.Subscriber.Reflex      ( ApiWidget
                                                , client
                                                , requestingJs
                                                , runApiWidget
                                                , ClientError
                                                , FreeClient
                                                , ClientError(..)
                                                )

import           Common.Auth

data Config = Config
  { configWebsocketUrl :: Text
  , configAceUrl :: Text
  , configMathjaxUrl :: Text
  , configMathjaxConfigUrl :: Text
  , configFiraUrl :: Text
  }
  deriving Generic

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

instance FromJSON Config where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON Config where
  toJSON     = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

getConfigFromPage :: MonadJSM m => m Config
getConfigFromPage = liftJSM $ do
  Just doc <- currentDocument
  Just hd  <- getHead doc
  node     <- maybe (fail "Config element not found") pure
    =<< querySelector hd ("#config" :: Text)
  fmap (decodeAesonOrFail . Text.encodeUtf8) (getInnerHTML node)
 where
  decodeAesonOrFail x = case eitherDecodeStrict' x of
    Left e ->
      error ("Frontend.Context.getConfig: error when decoding json: " ++ e)
    Right x' -> x'

getConfigFromFile :: FilePath -> IO Config
getConfigFromFile file = eitherDecodeFileStrict' file >>= either fail pure

getSelf :: Token -> FreeClient AuthUser
getSelf = client (Proxy :: Proxy AuthApi)

data FutureMaybe a
  = Present a
  | Absent
  | Unknown

futureToMaybe :: FutureMaybe a -> Maybe a
futureToMaybe (Present a) = Just a
futureToMaybe _           = Nothing

data AppConfig t = AppConfig
  { getConfig :: Config
  , getUser :: Dynamic t (FutureMaybe AuthUser)
  }

defaultAppConfig :: Reflex t => Config -> AppConfig t
defaultAppConfig cfg = AppConfig cfg (constDyn Unknown)

getUserNow :: Reflex t => AppConfig t -> Dynamic t (Maybe AuthUser)
getUserNow = fmap futureToMaybe . getUser

data GlobalAlert
  = LoginError (Text, Maybe Int)
  | WebsocketError
  deriving (Eq, Ord)

type AppT t m
  = EventWriterT t [GlobalAlert] (ReaderT (AppConfig t) (ApiWidget t m))

runAppT
  :: (PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m)
  => Config
  -> AppT t m a
  -> m (a, Event t [GlobalAlert])
runAppT cfg m = do
  ((y, ys), ws_err) <- runApiWidget (configWebsocketUrl cfg) $ do
    pb      <- getPostBuild
    usrEv   <- requestingJs (getSelf (Token "") <$ pb)
    dynUser <- holdDyn Unknown (either (const Absent) Present <$> usrEv)
    let userErr = fmapMaybe (either loginError (const Nothing)) usrEv
    (x, xs) <- runReaderT (runEventWriterT m)
      $ AppConfig { getConfig = cfg, getUser = dynUser }
    pure (x, xs <> userErr)
  pure (y, ys <> ([WebsocketError] <$ ws_err))

 where
  loginError e = case showClientError e of
    (_, Just 401) -> Nothing -- This is expected if the user is not (yet) logged in
    x             -> Just [LoginError x]


showClientError :: ClientError -> (Text, Maybe Int)
showClientError (FailureResponse rq rsp) =
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
              , if statusI == 401
                then Just "Please try reloading this page."
                else Nothing
              ]
  in  (msg, Just statusI)

showClientError (DecodeFailure x _) =
  ("The response decoding failed: " <> x, Nothing)
showClientError (UnsupportedContentType x _) =
  ("The content type " <> Text.pack (show x) <> " is not supported.", Nothing)
showClientError (InvalidContentTypeHeader _) =
  ("The content type header of the response is invalid.", Nothing)
showClientError (ConnectionError e) =
  ("A connection error occured: " <> Text.pack (displayException e), Nothing)

showUrl :: (Sub.BaseUrl, ByteString) -> Text
showUrl (_, p) = Text.decodeUtf8 p
