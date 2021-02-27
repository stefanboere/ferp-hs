{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Components.Alert
  ( AlertConfig(..)
  , alert
  , alertAppLevel
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
  (".alert" <> ".alert-app-level") ? do
    display flex
    flexDirection row
    alignItems baseline
    padding (rem (1 / 4)) (rem (1 / 2)) (rem (1 / 4)) (rem (1 / 2))

    Clay.span ? paddingAll (rem (1 / 4))

    ".icon" ? do
      position relative
      top (rem 0.2)

    a ? do
      textOverflow overflowEllipsis
      overflow hidden
      fontWeight (weight 600)
      fontSize (rem (12 / 16))

    (button <> a) ? do
      marginAll nil
      backgroundColor inherit

      hover & do
        backgroundColor inherit

    ".button-close" ? do
      paddingAll nil
      textOverflow overflowClip
      width (rem 1)
      position relative
      top (rem 0.2)
      ".icon" ? top nil

  ".alert-app-level" ? do

    (button # Clay.not ".button-close" <> a) ? do
      paddingRight (rem (3 / 4))
      paddingLeft (rem (3 / 4))
      paddingTop (rem (1 / 4))
      paddingBottom (rem (1 / 4))
      marginLeft (rem (1 / 4))
      marginRight (rem (1 / 4))
      fontColor inherit
      textDecoration none
      textTransform uppercase
      borderStyle solid
      borderWidth (px 1)
      borderRadiusAll (px 3)

    ".spacer" ? flexGrow 1

    ".success" & do
      backgroundColor (statusColor Success)
      fontColor nord6'
      "fill" -: showColor nord6'

    ".warning" & do
      backgroundColor (statusColor Warning)

    ".danger" & do
      backgroundColor (statusColor Danger)
      fontColor nord6'
      "fill" -: showColor nord6'

    ".info" & do
      backgroundColor (statusColor Info)
      fontColor nord6'
      "fill" -: showColor nord6'


  ".alert" ? do
    borderStyle solid
    borderWidth (px 1)
    marginBottom (rem (1 / 5))
    borderRadiusAll (px 3)

    a ? do
      fontColor nord1'

    (button <> a) ? do
      textDecoration underline
      textTransform none
      paddingRight (rem (2 / 4))
      paddingLeft (rem (2 / 4))
      height (rem 1)

    ".compactsize" & do
      paddingAll (rem (1 / 4))
      fontSize (rem (3 / 4))
      ".alert-message" ? do
        paddingTop nil
        paddingBottom nil

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
      flexGrow 1

  ".alert-message" ? do
    paddingLeft (rem (1 / 2))
    paddingRight (rem (1 / 2))

alertAppLevel
  :: (PostBuild t m, DomBuilder t m)
  => AlertConfig
  -> Dynamic t Text
  -> m a
  -> m (a, Event t ())
alertAppLevel AlertConfig {..} msg actions = elClass "div" classStr $ do
  elClass "span" "spacer" blank
  alertContent _alertConfig_status msg
  result <- actions
  elClass "span" "spacer" blank
  closeEv <- closeBtn
  pure (result, closeEv)
 where
  classStr = Text.toLower $ Text.unwords $ Prelude.filter
    (Prelude.not . Text.null)
    ["alert-app-level", pack . show $ _alertConfig_status]

alert
  :: (PostBuild t m, DomBuilder t m)
  => AlertConfig
  -> Dynamic t Text
  -> m a
  -> m (a, Event t ())
alert AlertConfig {..} msg actions = elClass "div" classStr $ do
  alertContent _alertConfig_status msg
  result  <- actions
  closeEv <- closeBtn
  pure (result, closeEv)
 where
  classStr = Text.toLower $ Text.unwords $ Prelude.filter
    (Prelude.not . Text.null)
    [ "alert"
    , pack . show $ _alertConfig_size
    , pack . show $ _alertConfig_status
    ]

alertContent
  :: (PostBuild t m, DomBuilder t m) => Status -> Dynamic t Text -> m ()
alertContent status msg = do
  icon def (statusStandardIcon status)
  elClass "span" "alert-message p3" (dynText msg)


closeBtn :: (PostBuild t m, DomBuilder t m) => m (Event t ())
closeBtn =
  btn def { _buttonConfig_priority = ButtonTertiary
          , _buttonConfig_size     = CompactSize
          , _buttonConfig_class    = "button-close"
          }
    $ icon def timesIcon
