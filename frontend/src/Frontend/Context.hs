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
import           Control.Lens                   ( (^.) )
import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as Text
import           GHC.Generics                   ( Generic )
import           Reflex.Dom                     ( Prerender
                                                , MonadHold
                                                )
import           Language.Javascript.JSaddle

import           Servant.Subscriber.Reflex      ( ApiWidget
                                                , runApiWidget
                                                )

newtype Config = Config
  { configWebsocketUrl :: Text
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
  doc       <- jsg ("document" :: Text)
  m         <- doc ^. js1 ("getElementById" :: Text) ("config" :: Text)
  htmlJs    <- m ^. js ("innerHtml" :: Text)
  Just html <- fromJSVal htmlJs
  either fail pure . eitherDecodeStrict' $ Text.encodeUtf8 html


getConfigFromFile :: FilePath -> IO Config
getConfigFromFile file = eitherDecodeFileStrict' file >>= either fail pure


type AppT t m = ReaderT Config (ApiWidget t m)

runAppT
  :: (MonadHold t m, MonadFix m, Prerender js t m)
  => Config
  -> AppT t m a
  -> m a
runAppT cfg m = runApiWidget (configWebsocketUrl cfg) (runReaderT m cfg)

