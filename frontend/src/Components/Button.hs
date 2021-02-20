{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Components.Button
  ( ButtonConfig(..)
  , ButtonPriority(..)
  , buttonStyle
  , btn
  )
where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( (&) )
import qualified Clay                           ( (&) )
import           Data.Default
import           Data.Text                      ( pack )
import qualified Data.Text                     as Text
import           Reflex
import           Reflex.Dom              hiding ( textInput
                                                , rangeInput
                                                )

import           Components.Class
import           Nordtheme

data ButtonPriority = ButtonPrimary Status
                    | ButtonSecondary
                    | ButtonTertiary
                    deriving (Eq, Ord, Show)

instance Default ButtonPriority where
  def = ButtonPrimary def


data ButtonConfig t = ButtonConfig
  { _buttonConfig_priority :: ButtonPriority
  , _buttonConfig_size :: ComponentSize
  , _buttonConfig_disabled :: Dynamic t Bool
  }

instance Reflex t => Default (ButtonConfig t) where
  def = ButtonConfig { _buttonConfig_priority = def
                     , _buttonConfig_size     = def
                     , _buttonConfig_disabled = constDyn False
                     }

buttonStyle :: Css
buttonStyle = Clay.button ? do
  Clay.display inlineBlock
  height (px 34)
  borderWidth nil
  borderRadiusAll (px 3)
  paddingRight (px 12)
  paddingLeft (px 12)
  marginRight (px 12)
  marginTop (px 6)
  marginBottom (px 6)
  cursor pointer
  textOverflow overflowEllipsis
  overflow hidden
  textAlign center
  textTransform uppercase
  verticalAlign middle

  ".compactsize" Clay.& do
    height (rem 1.5)

  ".info" Clay.& do
    background nord7'
    color nord1'
    hover Clay.& background (rgb 121 184 202)

  ".success" Clay.& do
    background nord14'
    borderColor nord14'
    hover Clay.& background (rgb 148 190 112)

  ".danger" Clay.& do
    background nord12'
    borderColor nord12'
    hover Clay.& background (rgb 207 120 93)

  ".warning" Clay.& do
    background nord13'
    borderColor nord13'
    hover Clay.& background (rgb 235 187 92)

  ".secondary" Clay.& do
    border solid 1 nord3'
    background white0'
    hoverSecondary

  ".tertiary" Clay.& do
    background white0'
    borderWidth nil
    hoverSecondary

 where
  hoverSecondary = hover Clay.& do
    borderColor (rgb 115 151 186)
    color (rgb 115 151 186)




btn :: DomBuilder t m => ButtonConfig t -> m a -> m (Event t ())
btn ButtonConfig {..} lbl = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: classStr) lbl
  pure $ domEvent Click e
 where
  classStr = Text.toLower $ Text.unwords
    [pack . show $ _buttonConfig_size, pack $ prioClass _buttonConfig_priority]
  prioClass (ButtonPrimary x) = show x
  prioClass ButtonSecondary   = "secondary"
  prioClass ButtonTertiary    = "tertiary"



