{-# LANGUAGE OverloadedStrings #-}
module Frontend
  ( main
  )
where

import           Clay
import           Data.Default
import           Data.Text                      ( pack )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Text.Lazy                 ( toStrict )
import           Reflex
import           Reflex.Dom

import           Components.Input
import           Nordtheme

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
  , ts_memo                   :: Maybe Material
  } deriving (Eq, Show)

main :: IO ()
main =
  mainWidgetWithCss (encodeUtf8 . toStrict $ renderWith compact [] css)
    $ el "form"
    $ do
        wall_thickness <- numberInput
          def { _numberInputConfig_precision = Just 3 }
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
        material <- selectInput def { _inputConfig_label = constDyn "Material"
                                    , _inputConfig_initialValue = Nothing
                                    }
        let dynTori =
              Torispherical
                <$> wall_thickness
                <*> outside_diameter
                <*> straight_flange_height
                <*> crown_radius
                <*> knuckle_radius
                <*> material
        dynText $ fmap (pack . show) dynTori

textFont :: Css
textFont = do
  fontSize (px 14)
  fontColor nord0'
  fontFamily ["Metropolis", "Fira Sans", "Helvetica"] [sansSerif]

css :: Css
css = do
  body ? do
    textFont
    background white0'

  inputStyle


