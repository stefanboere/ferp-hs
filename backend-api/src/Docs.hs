{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Docs
Description : Provides the docs route
-}
module Docs
  ( DocsApi
  , docsServer
  , SwaggerApi
  , swaggerServer
  )
where

import           Control.Monad.Reader           ( asks )
import qualified Data.ByteString.Lazy          as BL
import           Data.Swagger            hiding ( port )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Version
import           Servant
import           Servant.Auth.Docs              ( )
import           Servant.Auth.Swagger           ( )
import           Servant.Docs
import           Servant.Swagger
import           Servant.Swagger.UI
import           Servant.Crud.Server.QueryOperator
                                                ( )

import           Api                            ( api )
import           Context

-- * Markdown documentation

-- | Markdown documentation endpiont type
type DocsApi = Get '[PlainText] API

-- | Markdown documentation endpoint server
docsServer :: AppServer DocsApi
docsServer = do
  config <- asks getConfig
  return $ docsBS (configTitle config) (Text.lines (configDescription config))
 where
  docsBS :: Text -> [Text] -> API
  docsBS titl descr = docsWithIntros [intro] api
    where intro = DocIntro (Text.unpack titl) (map Text.unpack descr)

-- | Render API as markdown
instance MimeRender PlainText API where
  mimeRender _ = BL.fromStrict . Text.encodeUtf8 . Text.pack . markdown

instance ToSample () where
  toSamples _ = singleSample ()

-- * Swagger specs

-- | Swagger api endpoint
type SwaggerApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- | Swagger server
swaggerServer :: AppConfig -> Server SwaggerApi
swaggerServer cfg = swaggerSchemaUIServer
  $ (toSwagger api) { _swaggerInfo = getInfo . getConfig $ cfg }
 where

  getInfo :: Config -> Info
  getInfo config = Info
    { _infoTitle          = configTitle config
    , _infoDescription    = Just (configDescription config)
    , _infoTermsOfService = Nothing
    , _infoContact        = Nothing
    , _infoLicense        = Just
      (License "BSD-3-Clause"
               (Just (URL "https://opensource.org/licenses/BSD-3-Clause"))
      )
    , _infoVersion        = Text.pack . showVersion . configVersion $ config
    }
