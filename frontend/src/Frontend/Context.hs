{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Context
  ( Config(..)
  , AppT
  , getConfig
  , getConfigFromFile
  , runAppT
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader           ( ReaderT(..) )
import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as Text
import           GHC.Generics                   ( Generic )
import           GHCJS.DOM
import           GHCJS.DOM.Document             ( getHead )
import           GHCJS.DOM.Element              ( getInnerHTML )
import           GHCJS.DOM.ParentNode           ( querySelector )
import           Reflex.Dom                     ( Prerender
                                                , MonadHold
                                                )
import           Language.Javascript.JSaddle

import           Servant.Subscriber.Reflex      ( ApiWidget
                                                , runApiWidget
                                                )

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

getConfig :: MonadJSM m => m Config
getConfig = liftJSM $ do
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


type AppT t m = ReaderT Config (ApiWidget t m)

runAppT
  :: (MonadHold t m, MonadFix m, Prerender js t m)
  => Config
  -> AppT t m a
  -> m a
runAppT cfg m = runApiWidget (configWebsocketUrl cfg) (runReaderT m cfg)

