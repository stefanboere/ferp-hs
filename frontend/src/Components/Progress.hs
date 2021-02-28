{-# LANGUAGE OverloadedStrings #-}
module Components.Progress
  ( progressStyle
  , progressBar
  , progressBarLabel
  )
where

import           Prelude                 hiding ( rem )

import           Clay
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Reflex.Dom              hiding ( display
                                                , (&)
                                                )

import           Components.Class
import           Components.Input.Basic         ( randomId )
import           Nordtheme

progressStyle :: Css
progressStyle = do
  keyframesFromTo "clr-progress-looper" (left (pct (-100))) (left (pct 100))
  progressBarStyle

  ".progress" ? do
    display flex
    Clay.span ? do
      width (rem 3)
      textAlign end

progressBarStyle :: Css
progressBarStyle = progress ? do
  height (rem 1)
  width (pct 100)
  "-webkit-appearance" -: "none"
  "-moz-appearance" -: "none"
  "appearance" -: "none"
  overflow hidden

  borderStyle none

  "::-webkit-progress-bar" & do
    backgroundColor nord4'

  "::-webkit-progress-value" & do
    backgroundColor nord10'

  "::-moz-progress-bar" & do
    backgroundColor nord10'

  indeterminate & after & do
    display block
    content (stringContent "")
    position relative
    width (pct 75)
    left nil
    top (rem (-1))
    height (rem 1)
    backgroundColor nord10'
    animation "clr-progress-looper" 2 easeInOut 0 infinite normal none



-- | A bar filled up to a certain percentage.
--
-- If double is not between 0 and 1 an indeterminate progress bar is shown.
progressBar
  :: (DomBuilder t m, PostBuild t m)
  => Map Text Text
  -> Dynamic t (Maybe Double)
  -> m ()
progressBar otherAttrs val = elDynAttr "progress" (attrs <$> val)
  $ dynText (percentageLabel <$> val)

 where
  attrs v =
    otherAttrs
      <> ("max" =: "100")
      <> whenValid (\v' -> "value" =: percentageText v') v

percentageLabel :: Maybe Double -> Text
percentageLabel = whenValid ((<> "%") . percentageText)

whenValid :: (Ord a, Num a, Monoid m) => (a -> m) -> Maybe a -> m
whenValid f (Just v) | v >= 0 && v <= 1 = f v
                     | otherwise        = mempty
whenValid _ Nothing = mempty

percentageText :: Double -> Text
percentageText = Text.pack . show . rnd . (* 100)
 where
  rnd :: Double -> Integer
  rnd = Prelude.round


-- | A progress bar with label showing the percentage
progressBarLabel
  :: (DomBuilder t m, PostBuild t m)
  => Map Text Text
  -> Dynamic t (Maybe Double)
  -> m ()
progressBarLabel otherAttrs val = elClass "div" "progress" $ do
  progressBar otherAttrs val
  el "span" $ dynText (percentageLabel <$> val)
