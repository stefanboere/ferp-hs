{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
module Frontend.Truck
  ( truckHandler
  , truckLinks
  , TruckApi
  )
where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Proxy
import           GHC.Generics
import           Hledger
import           Math.LaTeX.Calculation
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                )

import           Reflex.Dom.HaTeX
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router
import           URI.ByteString

import           Components


-- brittany-disable-next-binding
type TruckApi = "truck" :> "torus" :> View

truckApi :: Proxy TruckApi
truckApi = Proxy

truckTorusLink :: Link
truckTorusLink = allLinks truckApi

truckLinks
  :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
truckLinks dynUri = safelinkGroup
  (text "Parametric modeling")
  [safelink dynUri truckTorusLink $ text "Torus"]

truckHandler :: WidgetConstraint js t m => RouteT TruckApi m (Event t URI)
truckHandler = torusHandler

torusHandler :: (WidgetConstraint js t m) => m (Event t URI)
torusHandler = do
  el "h1" $ text "Torus"

  elClass "div" "grid" $ do
    params <- card $ do
      cardHeader (text "Radii")
      cardContent $ el "form" $ do
        mayorEl <- labeled
          "Mayor radius"
          rangeAndNumberInput
          (inputConfig'
            (NumberRange { _numberRange_maxValue  = constDyn $ Just 1
                         , _numberRange_minValue  = constDyn $ Just 0.1
                         , _numberRange_precision = Just 2
                         }
            )
            "radius_mayor"
            (0.5 :: Double)
          )
        let mayor = _inputEl_value mayorEl
        minorEl <- labeled
          "Minor radius"
          rangeAndNumberInput
          (inputConfig'
            (NumberRange { _numberRange_maxValue  = mayor
                         , _numberRange_minValue  = constDyn $ Just 0.01
                         , _numberRange_precision = Just 2
                         }
            )
            "radius_minor"
            (0.1 :: Double)
          )
        let minor = _inputEl_value minorEl

        pure (liftA2 Torus <$> mayor <*> minor)

    card $ do
      cardHeader (text "Volumes")
      cardContent $ elLaTeXM (maybe errMsg torusVolumeCalc <$> params)

    cardTruckParam (fmap unTorus <$> params)

    cardTruckCosting params

  pure never

 where
  unTorus (Torus r1 r0) = (r1, r0)
  errMsg :: LaTeXM ()
  errMsg = document "Volumes could not be calculated"

data Torus a = Torus
  { _mayorRadius :: a
  , _minorRadius :: a
  }
  deriving (Generic1, Variables)

torusVolumeCalc :: Torus Double -> LaTeXM ()
torusVolumeCalc torus = document $ do
  "The volume of the torus is"
  _ <- formula torusSymbs 𝑉 torusVolume torus
  "And the surface area is given by"
  _ <- formula torusSymbs 𝐴 torusSurfaceArea torus
  pure ()
 where
  torusSymbs :: Torus (Expression' γ s² s¹ ζ)
  torusSymbs = Torus { _mayorRadius = 𝑅, _minorRadius = 𝑟 }

torusSurfaceArea :: Floating a => Torus a -> a
torusSurfaceArea Torus {..} = 4 * (pi ** 2) * _mayorRadius * _minorRadius

torusVolume :: Floating a => Torus a -> a
torusVolume Torus {..} = 2 * (pi ** 2) * _mayorRadius * (_minorRadius ** 2)

torusMaterialCosts :: Maybe (Torus Double) -> [BudgetPosting]
torusMaterialCosts Nothing = []
torusMaterialCosts (Just t) =
  [ def { _bp_account = "expenses:Materials:Base material"
        , _bp_comment = "Given by the volume 𝑉 "
        , _bp_amount  = mixed [eur (realToFrac (torusVolume t))]
        }
  , def { _bp_account = "expenses:Materials:Topping"
        , _bp_amount  = mixed [eur (realToFrac (torusSurfaceArea t))]
        }
  , def { _bp_account = "expenses:Materials:Box"
        , _bp_amount  = mixed [eur 0.05]
        }
  ]

torusLabourCosts :: Maybe (Torus Double) -> [BudgetPosting]
torusLabourCosts Nothing = []
torusLabourCosts (Just t) =
  [ def { _bp_account = "expenses:Labour:Production"
        , _bp_amount  = mixed [hrs (realToFrac (torusSurfaceArea t))]
        }
  , def { _bp_account = "expenses:Labour:Shop operation"
        , _bp_amount  = mixed [hrs 0.1]
        }
  ]

cardTruckCosting
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t (Maybe (Torus Double))
  -> m ()
cardTruckCosting params = cardCosting $ do
  dyn_ (errMsg <$> params)
  x <- costingGroup "Materials" $ torusMaterialCosts <$> params
  y <- costingGroup "Labour" $ torusLabourCosts <$> params
  pure (x + y)

 where
  errMsg (Just _) = pure ()
  errMsg Nothing  = do
    _ <- alert
      def { _alertConfig_size = CompactSize, _alertConfig_status = Warning }
      "Cost calculations cannot be performed"
      (pure ())
    pure ()
