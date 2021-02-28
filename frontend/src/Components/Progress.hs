{-# LANGUAGE OverloadedStrings #-}
module Components.Progress
  ( progressStyle
  , progressBar
  , progressBarLabel
  , SpinnerSize(..)
  , spinner
  )
where

import           Prelude                 hiding ( rem )

import           Clay
import           Data.Default
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Reflex.Dom              hiding ( display
                                                , (&)
                                                )

import           Components.Class
import           Nordtheme

progressStyle :: Css
progressStyle = do
  keyframesFromTo "clr-progress-looper" (left (pct (-100))) (left (pct 100))
  progressBarStyle

  keyframesFromTo "spin"
                  (transforms [rotate (deg 0)])
                  (transforms [rotate (deg 360)])
  spinnerStyle

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

data SpinnerSize = Inline
                 | Small
                 | Medium
                 | Large
                 deriving (Eq, Ord, Show)

instance Default SpinnerSize where
  def = Inline

spinnerStyle :: Css
spinnerStyle = do
  ".spinner" ? do
    display inlineBlock
    verticalAlign textBottom
    Clay.not ".spinner-inline" & do
      visibility hidden
      fontSize (px 1)
      letterSpacing (px (-1))

    before & do
      marginRight (rem (1 / 2))
      visibility visible
      display inlineBlock
      content (stringContent "")
      width (rem 1)
      height (rem 1)
      position relative
      borderRadiusAll (pct 50)
      border solid (px 2) nord4'
      borderBottomColor nord10'
      animation "spin" 1 linear 0 infinite normal none

  ".spinner-large" # before ? do
    width (rem 4)
    height (rem 4)
    borderWidth (px 5)

  ".spinner-medium" # before ? do
    width (rem 2)
    height (rem 2)
    borderWidth (px 3)

  ".spinner-inline" # before ? do
    top (rem 0.25)

spinner :: DomBuilder t m => SpinnerSize -> Text -> m ()
spinner sz loadingMsg =
  elClass "span" ("spinner spinner-" <> Text.toLower (Text.pack (show sz)))
    $ text loadingMsg

