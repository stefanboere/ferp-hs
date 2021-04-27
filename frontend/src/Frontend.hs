{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend
  ( main
  , mainWithHead
  , renderCss
  , mainBodyPrerender
  )
where

import           Clay                    hiding ( icon
                                                , id
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.ByteString                ( ByteString )
import           Data.Default
import           Data.Either                    ( fromRight )
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy
import           Data.Text.Lazy                 ( toStrict )
import qualified Data.Text.Lazy.IO             as LText
                                                ( putStr )
import           Language.Javascript.JSaddle.Types
                                                ( MonadJSM )
import           URI.ByteString
import           Reflex
import           Reflex.Dom              hiding ( rangeInput
                                                , Link(..)
                                                )
import           Reflex.Dom.Contrib.Router
                                         hiding ( URI )
import           Servant.API             hiding ( URI(..) )
import           Servant.Router

import           Components
import           Frontend.Container
import           Frontend.Core
import           Frontend.Input
import           Reflex.Markdown


main :: IO ()
main = mainWidget $ withHeader mainPage

mainWithHead :: IO ()
mainWithHead = mainWidgetWithHead headWidget $ withHeader mainPage

headWidget :: (DomBuilder t m) => m ()
headWidget = do
  el "style" $ text (toStrict $ renderWith compact [] css)
  fileDropzoneScript

renderCss :: IO ()
renderCss = LText.putStr $ renderWith compact [] css

mainBodyPrerender :: URI -> IO ByteString
mainBodyPrerender uri = snd <$> renderStatic (withHeader page)
 where
  page = do
    _ <- routeURI myApi handler . fixFragment $ uri
    pure $ constDyn uri

css :: Css
css = do
  appStyle
  inputStyle
  fileDropzoneStyle
  comboboxStyle
  buttonStyle
  accordionStyle
  cardStyle
  tableStyle
  alertStyle
  tagStyle
  progressStyle
  timelineStyle
  codeInputStyle
  markdownInputStyle

withHeader
  :: (MonadHold t m, MonadIO m, MonadFix m, PostBuild t m, DomBuilder t m)
  => m (Dynamic t URI)
  -> m ()
withHeader x = do
  rec dynUri <- app cfg (sideNav dynUri) (pure never) actions x
  pure ()

 where
  cfg = HeaderConfig { _headerConfig_appname           = constDyn "Ferp-hs"
                     , _headerConfig_navigationPattern = Sidenav
                     }
  actions = do
    _ <- btnDropdown def (icon def cogIcon) $ do
      accountEv <- ahref "#" (constDyn False) (text "Account")
      logoutEv  <- ahref "#" (constDyn False) (text "Logout")
      pure $ leftmost [accountEv, logoutEv]
    pure ()


mainPage
  :: ( WidgetConstraint js t m
     , MonadJSM (Performable m)
     , MonadJSM m
     , HasJSContext (Performable m)
     , HasJSContext m
     )
  => m (Dynamic t URI)
mainPage = do
  let routeHandler = route'
        (\_ uri -> fixFragment uri)
        (\uri -> (fixFragment uri, routeURI myApi handler . fixFragment $ uri))

  rec dynamicRoute <- routeHandler (switch (current changeRoute))
      routeWasSet  <- dyn (snd <$> dynamicRoute) -- Will fire on postbuild
      changeRoute  <- holdDyn never $ fmap (fromRight never) routeWasSet
  return $ fst <$> dynamicRoute

fixFragment :: URI -> URI
fixFragment uri@URI { uriFragment = frag, ..} =
  uri { uriPath = fromMaybe "/" frag }

-- brittany-disable-next-binding
type MyApi = InputApi
    :<|> CoreApi
    :<|> ContainerApi

myApi :: Proxy MyApi
myApi = Proxy

sideNav
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t ())
sideNav dynUri = leftmost
  <$> sequence [coreLinks dynUri, inputLinks dynUri, containerLinks dynUri]

handler :: WidgetConstraint js t m => RouteT MyApi m (Event t URI)
handler = inputHandler :<|> coreHandler :<|> containerHandler

