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
import           Data.Text                      ( Text
                                                , pack
                                                )
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
  , _buttonConfig_disabled :: Dynamic t Bool
  , _buttonConfig_class :: Text
  }

instance Reflex t => Default (ButtonConfig t) where
  def = ButtonConfig { _buttonConfig_priority = def
                     , _buttonConfig_disabled = constDyn False
                     , _buttonConfig_class    = ""
                     }

buttonStyle :: Css
buttonStyle = Clay.button ? do
  Clay.display inlineFlex
  alignItems center
  height (rem (3 / 2))
  backgroundColor inherit
  borderWidth nil
  borderRadiusAll (px 3)
  paddingRight (rem (3 / 4))
  paddingLeft (rem (3 / 4))
  marginRight (rem (3 / 4))
  marginTop (px 6)
  marginBottom (px 6)
  cursor pointer
  textOverflow overflowEllipsis
  overflow hidden
  textTransform uppercase
  verticalAlign middle
  color nord1'
  "fill" -: showColor nord1'

  star # firstChild <? marginLeft nil
  star # lastChild <? marginRight nil

  star <? do
    marginLeft (rem (1 / 8))
    marginRight (rem (1 / 8))

  ".primary" Clay.& ".badge" Clay.? do
    backgroundColor inherit
    color nord1'
    border solid (px 1) nord1'

  ".info" Clay.& do
    background nord7'
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
    hoverSecondary

  ".tertiary" Clay.& do
    borderWidth nil
    hoverSecondary

 where
  hoverSecondary = hover Clay.& do
    borderColor (rgb 115 151 186)
    color (rgb 115 151 186)
    "fill" -: showColor (rgb 115 151 186)


btn :: DomBuilder t m => ButtonConfig t -> m a -> m (Event t ())
btn ButtonConfig {..} lbl = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: classStr) lbl
  pure $ domEvent Click e
 where
  classStr = Text.toLower $ Text.unwords $ Prelude.filter
    (Prelude.not . Text.null)
    [pack $ prioClass _buttonConfig_priority, _buttonConfig_class]
  prioClass (ButtonPrimary x) = "primary " <> show x
  prioClass ButtonSecondary   = "secondary"
  prioClass ButtonTertiary    = "tertiary"



