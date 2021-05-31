{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend
  ( main
  , mainWithHead
  , renderCss
  , Api
  , api
  , handler
  , withHeader
  )
where

import           Clay                    hiding ( icon
                                                , id
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Either                    ( fromRight )
import           Data.Proxy
import           Data.Text.Lazy                 ( toStrict )
import qualified Data.Text.Lazy.IO             as LText
                                                ( putStr )
import           Language.Javascript.JSaddle.Types
                                                ( MonadJSM )
import           Reflex
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                )
import           Reflex.Dom.Contrib.Router
                                         hiding ( URI )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links                  ( linkURI )
import           Servant.Router
import           URI.ByteString

import           Components
import           Frontend.Api                   ( refreshAccessTokenEvery )
import           Frontend.Container
import           Frontend.Core
import           Frontend.Input
import           Frontend.Protected
import           Reflex.Markdown


main :: IO ()
main = mainWidget $ withHeader mainPage

mainWithHead :: IO ()
mainWithHead = mainWidgetWithHead headWidget $ do
  withHeader mainPage
  _ <- codeInputScripts
  pure ()

headWidget :: (DomBuilder t m) => m ()
headWidget = do
  el "style" $ text (toStrict $ renderWith compact [] css)
  fileDropzoneScript

renderCss :: IO ()
renderCss = LText.putStr $ renderWith compact [] css

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
  => (Event t Link -> m (Dynamic t URI))
  -> m ()
withHeader x = do
  rec (clickEv, dynUri) <- app cfg
                               (sideNav dynUri)
                               (pure never)
                               actions
                               (x clickEv)

  pure ()

 where
  cfg = HeaderConfig { _headerConfig_appname           = constDyn "Ferp-hs"
                     , _headerConfig_navigationPattern = Sidenav
                     }
  actions = do
    _ <- btnDropdown def (icon def cogIcon) $ do
      accountEv <- elAttrClick_ "a" ("href" =: "/auth/account") (text "Account")
      logoutEv  <- elAttrClick_ "a" ("href" =: "/auth/logout") (text "Logout")
      pure $ leftmost [accountEv, logoutEv]
    pure ()


mainPage
  :: ( WidgetConstraint js t m
     , MonadJSM (Performable m)
     , MonadJSM m
     , HasJSContext (Performable m)
     , HasJSContext m
     )
  => Event t Link
  -> m (Dynamic t URI)
mainPage setRouteExtEv = do
  let routeHandler =
        route' (\_ uri -> uri) (\uri -> (uri, routeURI api handler uri))

  rec
    dynamicRoute <- routeHandler changeRoute
    routeSetEvEv <- dyn (snd <$> dynamicRoute) -- Will fire on postbuild
    routeSetEv   <- switchHold never $ fmap (fromRight never) routeSetEvEv
    let changeRoute =
          leftmost [coerceUri . linkURI <$> setRouteExtEv, routeSetEv]

  refreshAccessTokenEvery 300

  return $ fst <$> dynamicRoute

-- brittany-disable-next-binding
type Api = InputApi
    :<|> CoreApi
    :<|> ContainerApi
    :<|> ProtectedApi

api :: Proxy Api
api = Proxy

sideNav
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
sideNav dynUri = leftmost <$> sequence
  [ coreLinks dynUri
  , inputLinks dynUri
  , containerLinks dynUri
  , protectedLinks dynUri
  ]

handler :: WidgetConstraint js t m => RouteT Api m (Event t URI)
handler =
  inputHandler :<|> coreHandler :<|> containerHandler :<|> protectedHandler

