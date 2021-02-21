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
import           Data.Default
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Text.Lazy                 ( toStrict )
import           URI.ByteString
import           Reflex
import           Reflex.Dom              hiding ( rangeInput )
import           Reflex.Dom.Contrib.Router
                                         hiding ( URI )
import           Servant.API             hiding ( URI(..) )
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

withHeader :: (PostBuild t m, DomBuilder t m) => m () -> m ()
withHeader = app cfg sideNav (pure ()) actions

 where
  cfg = HeaderConfig { _headerConfig_appname           = constDyn "Ferp-hs"
                     , _headerConfig_navigationPattern = Sidenav
                     }
  actions = ahref "#" (constDyn False) $ icon def cogIcon

  sideNav = navGroup (text "Input elements") $ do
    ahref "#/input/basic" (constDyn False) $ text "Basic"
    ahref "#/input/button" (constDyn False) $ text "Button"

mainPage :: forall t m . MonadWidget t m => m ()
mainPage = do
  let routeHandler = route' (\_ uri -> fixFragment uri)
                            (routeURI myApi handler . fixFragment)

  rec dynamicRoute <- routeHandler (switch (current changeRoute))
      routeWasSet  <- dyn dynamicRoute -- Will fire on postbuild
      changeRoute  <- holdDyn never $ fmap (either (const never) id) routeWasSet
  return ()

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

myApi :: Proxy MyApi
myApi = Proxy

handler :: MonadWidget t m => RouteT MyApi m (Event t URI)
handler = inputBasic :<|> inputButton
 where
  inputBasic  = formTest >> pure never
  inputButton = text "TBD" >> pure never


css :: Css
css = do
  appStyle
  inputStyle
  buttonStyle


