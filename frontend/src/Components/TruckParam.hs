{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Components.TruckParam
  ( cardTruckParam
  , truckParamStyle
  )
where

import           Clay                    hiding ( id
                                                , call
                                                , div
                                                )
import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( void )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Lens                   ( (^.) )
import           Data.Text                      ( Text )
import           Language.Javascript.JSaddle
                                         hiding ( (#) )
import qualified GHCJS.DOM.Types               as DOM
import           GHCJS.DOM                      ( currentWindowUnchecked )
import           GHCJS.DOM.WindowOrWorkerGlobalScope
                                                ( setTimeout_ )
import           GHCJS.DOM.Types                ( uncheckedCastTo
                                                , HTMLElement(..)
                                                )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                )

import           Components.Card

truckParamStyle :: Css
truckParamStyle = ".truck-param" ? do
  backgroundColor
    $ rgb (45 * 255 `div` 455) (52 * 255 `div` 455) (64 * 255 `div` 455)

  canvas ? do
    width (pct 100)
    height (pct 100)

cardTruckParam
  :: (DomBuilder t m, Prerender js t m)
  => Dynamic t (Maybe (Double, Double))
  -> m ()
cardTruckParam params =
  card
    $ elClass "div" "truck-param card-nopadding"
    $ prerender_ (pure ())
    $ truckParamJs params

truckParamJs
  :: ( MonadJSM m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadJSM (Performable m)
     , MonadFix m
     , MonadHold t m
     )
  => Dynamic t (Maybe (Double, Double))
  -> m ()
truckParamJs dynParams' = do
  uniqParams    <- holdUniqDyn dynParams'
  dynParams     <- improvingMaybe uniqParams
  (canvasEl, _) <- el' "canvas" blank
  let c = uncheckedCastTo HTMLElement $ _element_raw canvasEl
  (loadEv, raiseLoad) <- newTriggerEvent
  liftJSM $ do
    window <- currentWindowUnchecked
    rec tryInit <- function $ \_ _ _ -> do
          truckParam <- jsg ("TruckParam" :: Text)
          undef      <- valIsUndefined truckParam
          if undef
            then do
              setTimeout_ window tryInit (Just 50)
            else do
              app' <- new truckParam (toJSVal c)
              liftIO $ raiseLoad app'

    _ <- call (makeObject (toJSVal tryInit)) global ()
    pure ()

  dynApp <- holdDyn Nothing (Just <$> loadEv)
  let ev' = leftmost
        [ attachWithMaybe (liftA2 (,)) (current dynApp) (updated dynParams)
        , attachWithMaybe (flip (liftA2 (,)))
                          (current dynParams)
                          (updated dynApp)
        ]
  ev <- throttle 0.1 ev'

  _  <- performEventAsync $ ffor ev $ \(truck_param, (x, y)) _ -> do
    liftJSM $ void $ truck_param ^. js2 ("build_torus" :: Text) x y
  pure ()


