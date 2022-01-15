{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Truck
  ( truckHandler
  , truckLinks
  , TruckApi
  )
where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Proxy
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                )

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

        pure (liftA2 (,) <$> mayor <*> minor)

    cardTruckParam params

  pure never
