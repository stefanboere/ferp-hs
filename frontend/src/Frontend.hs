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
  , handlerOffline
  , withHeader
  , Config(..)
  , getConfigFromFile
  )
where

import           Clay                    hiding ( icon
                                                , id
                                                )
import           Control.Monad                  ( unless )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader           ( ReaderT(..) )
import           Data.Default
import           Data.Either                    ( fromRight )
import           Data.Maybe                     ( fromMaybe )
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
import           Frontend.Context
import           Frontend.Container
import           Frontend.Core
import           Frontend.Input
import           Frontend.Crud


main :: IO ()
main = mainWidget $ do
  cfg <- getConfig
  runAppT cfg $ withHeader (mainPage False)

mainWithHead :: IO ()
mainWithHead = do
  cfg <- getConfigFromFile "config.json"
  mainWidgetWithHead headWidget $ runAppT cfg $ do
    withHeader' True (mainPage True)
    elAttr
      "link"
      (  "href"
      =: configFiraUrl cfg
      <> "rel"
      =: "stylesheet"
      <> "type"
      =: "text/css"
      )
      blank
    script' (configAceUrl cfg)
    deferScript (configMathjaxConfigUrl cfg)
    deferScript (configMathjaxUrl cfg)
 where
  script' uri = elAttr "script" ("src" =: uri <> "async" =: "") blank
  deferScript uri = elAttr "script" ("src" =: uri <> "defer" =: "") blank

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
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m)
  => (Event t Link -> m (Dynamic t URI))
  -> m ()
withHeader = withHeader' False

withHeader'
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m)
  => Bool
  -> (Event t Link -> m (Dynamic t URI))
  -> m ()
withHeader' useFragment x = do
  rec (clickEv, dynUri) <- app cfg
                               (sideNav dynUri)
                               (pure never)
                               actions
                               (x clickEv)

  pure ()

 where
  cfg = HeaderConfig
    { _headerConfig_appname           = constDyn "Ferp-hs"
    , _headerConfig_navigationPattern = Sidenav
    , _headerConfig_homePageUrl       = if useFragment then "#/" else "/"
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
  => Bool
  -> Event t Link
  -> AppT t m (Dynamic t URI)
mainPage useFragment setRouteExtEv = do
  let routeHandler =
        route' encode (\uri -> (decode uri, routeURI api handler (decode uri)))

  rec
    dynamicRoute <- routeHandler changeRoute
    routeSetEvEv <- dyn (snd <$> dynamicRoute) -- Will fire on postbuild
    routeSetEv   <- switchHold never $ fmap (fromRight never) routeSetEvEv
    let changeRoute =
          leftmost [coerceUri . linkURI <$> setRouteExtEv, routeSetEv]

  unless useFragment $ refreshAccessTokenEvery 300

  return $ fst <$> dynamicRoute
 where
  decode :: URI -> URI
  decode uri = if useFragment
    then uri { uriPath = fromMaybe "" $ uriFragment uri, uriFragment = Nothing }
    else uri

  encode :: URI -> URI -> URI
  encode uri0 uri1 = if useFragment
    then uri0 { uriQuery = uriQuery uri1, uriFragment = Just $ uriPath uri1 }
    else uri0 { uriQuery    = uriQuery uri1
              , uriFragment = uriFragment uri1
              , uriPath     = uriPath uri1
              }

-- brittany-disable-next-binding
type Api = View
    :<|> InputApi
    :<|> CoreApi
    :<|> ContainerApi
    :<|> CrudApi

api :: Proxy Api
api = Proxy

sideNav
  :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
sideNav dynUri = leftmost <$> sequence
  [coreLinks dynUri, inputLinks dynUri, containerLinks dynUri, crudLinks dynUri]

homeHandler :: (DomBuilder t m) => m (Event t URI)
homeHandler = pure never

handler :: WidgetConstraint js t m => RouteT Api (AppT t m) (Event t URI)
handler =
  homeHandler
    :<|> inputHandler
    :<|> coreHandler
    :<|> containerHandler
    :<|> crudHandler

handlerOffline
  :: WidgetConstraint js t m => RouteT Api (ReaderT Config m) (Event t URI)
handlerOffline = hoistRoute (Proxy :: Proxy Api) runApi handler
  where runApi m = ReaderT $ \cfg -> runAppT cfg m

