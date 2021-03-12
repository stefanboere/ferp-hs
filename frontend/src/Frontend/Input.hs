{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend.Input
  ( inputHandler
  , inputLinks
  , InputApi
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Proxy
import           Data.Text                      ( Text
                                                , pack
                                                )
import           URI.ByteString
import           Reflex
import           Reflex.Dom              hiding ( rangeInput
                                                , Link(..)
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router

import           Components

-- brittany-disable-next-binding
type InputApi = "input" :> "basic" :> View
           :<|> "input" :> "checkbox" :> View

inputApi :: Proxy InputApi
inputApi = Proxy

inputBasicLink, inputCheckboxLink :: Link
inputBasicLink :<|> inputCheckboxLink = allLinks inputApi

inputLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t ())
inputLinks dynUri = safelinkGroup
  (text "Input elements")
  [ safelink dynUri inputBasicLink $ text "Basic"
  , safelink dynUri inputCheckboxLink $ text "Checkbox"
  ]

inputHandler :: MonadWidget t m => RouteT InputApi m (Event t URI)
inputHandler = (formTest >> pure never) :<|> checkboxHandler

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


checkboxHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
checkboxHandler = do
  el "h1" $ text "Checkbox"

  el "form" $ do
    _ <- checkboxInput (inputConfig False)
      { _inputConfig_label = constDyn "I agree to the terms"
      }

    _ <- checkboxesInput (inputConfig [M14307])
      { _inputConfig_label  = constDyn "Material"
      , _inputConfig_status = constDyn $ InputError "Error"
      }

    _ <- checkboxesInput (inputConfig [M14307])
      { _inputConfig_label  = constDyn "Disabled"
      , _inputConfig_status = constDyn InputDisabled
      }

    _ <- checkboxesInput (inputConfig [M14307])
      { _inputConfig_label  = constDyn "Success"
      , _inputConfig_status = constDyn $ InputSuccess "Success message"
      }
    pure ()

  elClass "form" "vertical" $ do
    _ <- checkboxesInputLbl
      (\() -> "I agree")
      (inputConfig [()]) { _inputConfig_label = constDyn "Terms and conditions"
                         }

    _ <- checkboxesInput (inputConfig [M14307])
      { _inputConfig_label = constDyn "Vertical layout"
      }
    pure ()

  pure never
