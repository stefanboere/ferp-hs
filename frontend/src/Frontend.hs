{-# LANGUAGE OverloadedStrings #-}
module Frontend
  ( main
  )
where

import           Clay
import           Control.Monad.Fix              ( MonadFix )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Text.Lazy                 ( toStrict )
import           Reflex
import           Reflex.Dom
import           Text.Read                      ( readMaybe )

import           Components.Input
import           Nordtheme

data Overridable a = Overridden a | Default a

data Torispherical t = Torispherical
  { ts_wall_thickness         :: t Double
  , ts_outside_diameter       :: t Double
  , ts_straight_flange_height :: t Double
  , ts_crown_radius           :: Overridable (t Double)
  , ts_knucke_radius          :: Overridable (t Double)
  }

main :: IO ()
main =
  mainWidgetWithCss (encodeUtf8 . toStrict $ renderWith compact [] css)
    $ el "form"
    $ do
        nx <- numberInput def { _inputConfig_label = constDyn "Wall thickness"
                              , _inputConfig_initialValue = 0 :: Double
                              }
        dynText $ fmap (pack . show) nx

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



