{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Components.Alert
  ( AlertConfig(..)
  , alert
  , alertStyle
  )
where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( icon )
import           Data.Default
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
import           Reflex.Dom              hiding ( display
                                                , button
                                                , (&)
                                                )

import           Components.Button
import           Components.Class
import           Components.Icon
import           Nordtheme

data AlertConfig = AlertConfig
  { _alertConfig_status :: Status
  , _alertConfig_size :: ComponentSize
  }

instance Default AlertConfig where
  def = AlertConfig { _alertConfig_status = def, _alertConfig_size = def }

alertStyle :: Css
alertStyle = do
  ".alert" ? do
    display flex
    flexDirection row
    alignItems baseline
    borderStyle solid
    borderWidth (px 1)
    borderRadiusAll (px 3)
    marginBottom (rem (1 / 5))
    paddingAll (rem (1 / 2))

    Clay.span ? flexGrow 1
    ".icon" ? do
      position relative
      top (rem 0.2)

    a ? do
      fontColor nord1'
      textOverflow overflowEllipsis
      overflow hidden
      fontWeight (weight 600)
      fontSize (rem (12 / 16))

    (button <> a) ? do
      paddingRight (rem (2 / 4))
      paddingLeft (rem (2 / 4))
      backgroundColor inherit
      textDecoration underline
      textTransform none
      marginAll nil
      height (rem 1)

      hover & do
        backgroundColor inherit

    ".button-close" ? do
      paddingAll nil
      textOverflow overflowClip
      width (rem 1)
      position relative
      top (rem 0.2)
      ".icon" ? top nil

    ".compactsize" & do
      paddingAll (rem (1 / 4))
      fontSize (rem (3 / 4))
      ".alert-message" ? do
        paddingLeft (rem (1 / 4))
        paddingRight (rem (1 / 4))

    ".success" & do
      borderColor (statusColor Success)
      backgroundColor $ lighten (8 / 10) nord14'

    ".warning" & do
      borderColor $ darken (2 / 10) (statusColor Warning)
      backgroundColor $ lighten (8 / 10) (statusColor Warning)

    ".danger" & do
      borderColor (statusColor Danger)
      backgroundColor $ lighten (9 / 10) (statusColor Danger)

    ".info" & do
      borderColor (statusColor Info)
      backgroundColor $ lighten (9 / 10) (statusColor Info)

  ".alert-message" ? do
    paddingLeft (rem (1 / 2))
    paddingRight (rem (1 / 2))

alert
  :: (PostBuild t m, DomBuilder t m)
  => AlertConfig
  -> Dynamic t Text
  -> m a
  -> m (a, Event t ())
alert AlertConfig {..} msg actions = elClass "div" classStr $ do
  icon def (statusStandardIcon _alertConfig_status)
  elClass "span" "alert-message p3" (dynText msg)
  result  <- actions
  closeEv <-
    btn def { _buttonConfig_priority = ButtonTertiary
            , _buttonConfig_size     = CompactSize
            , _buttonConfig_class    = "button-close"
            }
      $ icon def timesIcon
  pure (result, closeEv)
 where
  classStr = Text.toLower $ Text.unwords $ Prelude.filter
    (Prelude.not . Text.null)
    [ "alert"
    , pack . show $ _alertConfig_size
    , pack . show $ _alertConfig_status
    ]
