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
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Concurrent.MVar
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
  :: ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , MonadHold t m
     , MonadFix m
     , Prerender js t m
     )
  => Dynamic t (Maybe (Double, Double))
  -> m ()
cardTruckParam params = do
  uniqParams <- holdUniqDyn params
  ev         <- throttle 0.1 (fmapMaybe id $ updated uniqParams)
  pb'        <- getPostBuild
  pb         <- delay 5 pb'
  card
    $ elClass "div" "truck-param card-nopadding"
    $ prerender_ (pure ())
    $ truckParamJs
    $ leftmost [ev, tagMaybe (current params) pb]

truckParamJs
  :: ( MonadJSM m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadJSM (Performable m)
     )
  => Event t (Double, Double)
  -> m ()
truckParamJs ev = do
  (canvasEl, _) <- el' "canvas" blank
  let c = uncheckedCastTo HTMLElement $ _element_raw canvasEl
  truck_param_mvar <- liftJSM $ do
    window <- currentWindowUnchecked
    result <- liftIO newEmptyMVar
    rec tryInit <- function $ \_ _ _ -> do
          truckParam <- jsg ("TruckParam" :: Text)
          undef      <- valIsUndefined truckParam
          if undef
            then do
              setTimeout_ window tryInit (Just 50)
            else do
              app' <- new truckParam (toJSVal c)
              liftIO $ putMVar result app'

    _ <- call (makeObject (toJSVal tryInit)) global ()
    pure result

  _ <- performEventAsync $ ffor ev $ \(x, y) _ -> do
    liftJSM $ do
      truck_param <- liftIO $ readMVar truck_param_mvar
      _           <- truck_param ^. js2 ("build_torus" :: Text) x y
      pure ()
  pure ()


