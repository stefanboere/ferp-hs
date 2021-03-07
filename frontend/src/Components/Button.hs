{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Components.Button
  ( ButtonConfig(..)
  , ButtonPriority(..)
  , ActionState(..)
  , buttonStyle
  , btn
  , btnGroup
  )
where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( (&)
                                                , icon
                                                )
import qualified Clay                           ( (&) )
import           Data.Default
import           Data.Map                      as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
import           Reflex
import           Reflex.Dom              hiding ( textInput
                                                , rangeInput
                                                )

import           Components.Class
import           Components.Icon
import           Components.Progress
import           Nordtheme

data ButtonPriority = ButtonPrimary Status
                    | ButtonSecondary
                    | ButtonTertiary
                    deriving (Eq, Ord, Show)

instance Default ButtonPriority where
  def = ButtonPrimary def

data ActionState = ActionAvailable -- ^ The call to action is available (this is the default state)
                 | ActionLoading -- ^ Communicate that the app is working on the call-to-action
                 | ActionSuccess -- ^ The call-to-action has completed successfully and can not (yet) be called again
                 | ActionError -- ^ The call-to-action failed and can not (yet) be called again
                 | ActionDisabled -- ^ The call to action is currently unavailable
                 deriving(Eq, Show)

instance Default ActionState where
  def = ActionAvailable

data ButtonConfig t = ButtonConfig
  { _buttonConfig_priority :: ButtonPriority
  , _buttonConfig_state :: Dynamic t ActionState
  , _buttonConfig_class :: Text
  }

instance Reflex t => Default (ButtonConfig t) where
  def = ButtonConfig { _buttonConfig_priority = def
                     , _buttonConfig_state    = constDyn def
                     , _buttonConfig_class    = ""
                     }

buttonStyle :: Css
buttonStyle = buttonStyle' <> btnGroupStyle

buttonStyle' :: Css
buttonStyle' = Clay.button ? do
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
    hover Clay.& enabled Clay.& background (rgb 121 184 202)

  ".success" Clay.& do
    background nord14'
    borderColor nord14'
    hover Clay.& enabled Clay.& background (rgb 148 190 112)

  ".danger" Clay.& do
    background nord12'
    borderColor nord12'
    hover Clay.& enabled Clay.& background (rgb 207 120 93)

  ".warning" Clay.& do
    background nord13'
    borderColor nord13'
    hover Clay.& enabled Clay.& background (rgb 235 187 92)

  ".secondary" Clay.& do
    border solid 1 nord3'
    hoverSecondary

  ".tertiary" Clay.& do
    borderWidth nil
    hoverSecondary

  ".spinner" ? do
    width (rem 1)

  ".spinner" # before ? do
    width (rem (3 / 4))
    height (rem (3 / 4))
    marginTop (rem (-3 / 4))


  disabled Clay.& do
    cursor notAllowed
    backgroundColor disabledBg
    borderColor disabledBg
    fontColor nord0'
    (".spinner" <> ".action-icon") |+ ".icon" ? do
      important $ Clay.display none

 where
  disabledBg     = lighten 0.5 grey0'

  hoverSecondary = hover Clay.& enabled Clay.& do
    borderColor (rgb 115 151 186)
    color (rgb 115 151 186)
    "fill" -: showColor (rgb 115 151 186)
    ".badge" ? backgroundColor (rgb 115 151 186)

btn
  :: (PostBuild t m, DomBuilder t m) => ButtonConfig t -> m a -> m (Event t ())
btn ButtonConfig {..} lbl = do
  (e, _) <- elDynAttr' "button"
                       (mkAttrs <$> _buttonConfig_state)
                       (dyn (stateIcon <$> _buttonConfig_state) >> lbl)
  pure
    $ gate (current ((== ActionAvailable) <$> _buttonConfig_state))
    $ domEvent Click e
 where
  classStr = Text.toLower $ Text.unwords $ Prelude.filter
    (Prelude.not . Text.null)
    [pack $ prioClass _buttonConfig_priority, _buttonConfig_class]
  prioClass (ButtonPrimary x) = "primary " <> show x
  prioClass ButtonSecondary   = "secondary"
  prioClass ButtonTertiary    = "tertiary"

  stateIcon ActionError = icon
    def { _iconConfig_status = constDyn (Just Danger)
        , _iconConfig_class  = Just "action-icon"
        }
    errorStandardIcon
  stateIcon ActionSuccess = icon
    def { _iconConfig_status = constDyn (Just Success)
        , _iconConfig_class  = Just "action-icon"
        }
    checkIcon
  stateIcon ActionLoading = spinner def ""
  stateIcon _             = pure ()

  mkAttrs state = Map.fromList $ catMaybes
    [ Just ("type", "button")
    , Just ("class", classStr)
    , if state == ActionAvailable then Nothing else Just ("disabled", "")
    ]

btnGroupStyle :: Css
btnGroupStyle = ".button-group" ? do
  Clay.display inlineBlock
  marginRight (rem (3 / 4))

  Clay.button # firstChild <? do
    borderTopLeftRadius (px 3) (px 3)
    borderBottomLeftRadius (px 3) (px 3)

  Clay.button # lastChild <? do
    borderTopRightRadius (px 3) (px 3)
    borderBottomRightRadius (px 3) (px 3)

  Clay.button <? do
    marginRight nil
    borderRadiusAll nil

  ".primary" <? do
    marginRight (px 1)

  ".secondary" <? do
    marginRight (px (-1))

  ".secondary" # hover |+ star ? do
    borderLeftColor (rgb 115 151 186)

btnGroup :: DomBuilder t m => m () -> m ()
btnGroup = elClass "div" "button-group"
