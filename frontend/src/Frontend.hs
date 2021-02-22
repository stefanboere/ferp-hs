{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend
  ( main
  )
where

import           Clay                    hiding ( icon
                                                , id
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.ByteString.Char8         as B
                                                ( pack )
import           Data.Default
import           Data.Either                    ( fromRight )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Any(..) )
import           Data.Proxy
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Text.Lazy                 ( toStrict )
import           URI.ByteString
import           Reflex
import           Reflex.Dom              hiding ( rangeInput
                                                , Link(..)
                                                )
import           Reflex.Dom.Contrib.Router
                                         hiding ( URI )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import qualified Servant.Links                 as L
import           Servant.Router

import           Components

instance Default Text where
  def = mempty

data Material = M14404 | M14307 deriving (Eq, Show, Enum, Bounded)

instance Default Material where
  def = M14404

instance HasLabel Material where
  toLabel M14404 = "1.4404"
  toLabel M14307 = "1.4307"

data Torispherical = Torispherical
  { ts_wall_thickness         :: Maybe Double
  , ts_outside_diameter       :: Maybe Double
  , ts_straight_flange_height :: Maybe Double
  , ts_crown_radius           :: Maybe (Overridable Double)
  , ts_knuckle_radius         :: Maybe (Overridable Double)
  , ts_material               :: Maybe Material
  , ts_memo                   :: Text
  } deriving (Eq, Show)

main :: IO ()
main = mainWidgetWithCss (encodeUtf8 . toStrict $ renderWith compact [] css)
  $ withHeader mainPage

css :: Css
css = do
  appStyle
  inputStyle
  buttonStyle

withHeader
  :: (MonadIO m, MonadFix m, PostBuild t m, DomBuilder t m)
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
    _ <- ahref "#" (constDyn False) $ icon def cogIcon
    pure ()


safelink
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> Link
  -> m ()
  -> m (Dynamic t Bool, Event t ())
safelink dynLoc lnk cnt = do
  closeEv <- ahref frag isActiveDyn cnt
  pure (isActiveDyn, closeEv)
 where
  isActiveDyn = (frag' ==) . uriPath <$> dynLoc
  frag'       = "/" <> B.pack (L.uriPath uri)
  uri         = linkURI lnk
  frag        = "#/" <> pack (show uri)

-- | A group of links which automatically opens if one child is active
safelinkGroup
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => m ()
  -> [m (Dynamic t Bool, Event t ())]
  -> m (Event t ())
safelinkGroup lbl childs = do
  rec closeEvs <- navGroup (leftmost [initActive, updated anyActive]) lbl
        $ sequence childs

      postBuild <- getPostBuild
      let anyActive  = fmap getAny $ mconcat $ fmap (fmap Any . fst) closeEvs
      let initActive = tagPromptlyDyn anyActive postBuild
  pure $ leftmost $ fmap snd closeEvs

mainPage :: forall t m . MonadWidget t m => m (Dynamic t URI)
mainPage = do
  let routeHandler = route'
        (\_ uri -> fixFragment uri)
        (\uri -> (fixFragment uri, routeURI myApi handler . fixFragment $ uri))

  rec dynamicRoute <- routeHandler (switch (current changeRoute))
      routeWasSet  <- dyn (snd <$> dynamicRoute) -- Will fire on postbuild
      changeRoute  <- holdDyn never $ fmap (fromRight never) routeWasSet
  return $ fst <$> dynamicRoute

 where
  fixFragment :: URI -> URI
  fixFragment uri@URI { uriFragment = frag, ..} =
    uri { uriPath = fromMaybe "/" frag }

formTest
  :: (MonadIO m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m ()
formTest = el "form" $ do
  wall_thickness <- numberInput
    def { _numberInputConfig_precision = Just 3
        , _numberInputConfig_minValue  = constDyn (Just 0)
        }
    def { _inputConfig_label        = constDyn "Wall thickness"
        , _inputConfig_initialValue = 0 :: Double
        }
  outside_diameter <- numberInput
    def { _numberInputConfig_precision = Just 3 }
    def { _inputConfig_label        = constDyn "Outside diameter"
        , _inputConfig_initialValue = 0 :: Double
        }
  straight_flange_height <- numberInput
    def { _numberInputConfig_precision = Just 3 }
    def { _inputConfig_label        = constDyn "Straight flange height"
        , _inputConfig_initialValue = 0 :: Double
        }
  crown_radius <- overridableNumberInput
    (fmapMaybe Prelude.id $ updated outside_diameter)
    def { _inputConfig_label        = constDyn "Crown radius"
        , _inputConfig_initialValue = Overridable (0 :: Double) Nothing
        }
  knuckle_radius <- overridableNumberInput
    (fmapMaybe (fmap (/ 10)) $ updated outside_diameter)
    def { _inputConfig_label        = constDyn "Knuckle radius"
        , _inputConfig_initialValue = Overridable (0 :: Double) Nothing
        }
  material <- selectInput def { _inputConfig_label        = constDyn "Material"
                              , _inputConfig_initialValue = Nothing
                              }
  memo <- textAreaInput def { _inputConfig_label        = constDyn "Memo"
                            , _inputConfig_initialValue = mempty
                            }
  let dynTori =
        Torispherical
          <$> wall_thickness
          <*> outside_diameter
          <*> straight_flange_height
          <*> crown_radius
          <*> knuckle_radius
          <*> material
          <*> memo
  dynText $ fmap (pack . show) dynTori

-- brittany-disable-next-binding
type MyApi = "input" :> "basic" :> View
        :<|> "input" :> "button" :> View
        :<|> "container" :> "tab" :> View

myApi :: Proxy MyApi
myApi = Proxy

inputBasicLink, inputButtonLink, containerTabLink :: Link
inputBasicLink :<|> inputButtonLink :<|> containerTabLink = allLinks myApi

sideNav
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t ())
sideNav dynUri = leftmost <$> sequence
  [ safelinkGroup
    (text "Input elements")
    [ safelink dynUri inputBasicLink $ text "Basic"
    , safelink dynUri inputButtonLink $ text "Button"
    ]
  , safelinkGroup (text "Containers")
                  [safelink dynUri containerTabLink $ text "Tab"]
  ]

containerTab
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
containerTab = do
  el "h1" $ text "Tab"
  tabs
    (  1
    =: ("Tab 1", text "Tab 1 content")
    <> (2 :: Int)
    =: ("Tab 2", text "Tab 2 content")
    )
  pure never

handler :: MonadWidget t m => RouteT MyApi m (Event t URI)
handler = inputBasic :<|> inputButton :<|> containerTab
 where
  inputBasic  = formTest >> pure never
  inputButton = text "TBD" >> pure never



