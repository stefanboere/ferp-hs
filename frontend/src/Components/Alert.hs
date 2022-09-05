{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Components.Alert
  ( AlertConfig(..)
  , alert
  , alerts
  , alertAppLevel
  , alertsAppLevel
  , alertStyle
  , alertLightweight
  , closeBtn
  )
where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( icon )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Default
import qualified Data.Map.Strict               as Map
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
  (".alert" <> ".alert-app-level" <> ".alert-light") ? do
    display flex
    flexDirection row
    alignItems center
    padding (rem (1 / 4)) (rem (1 / 2)) (rem (1 / 4)) (rem (1 / 2))

    Clay.span ? paddingAll (rem (1 / 4))

    ".icon" ? do
      position relative
      alignSelf flexStart
      top (rem (1 / 4))

    a ? do
      textOverflow overflowEllipsis
      overflow hidden
      fontWeight (weight 600)
      fontSize (rem (12 / 16))

    (button <> a) ? do
      marginAll nil
      backgroundColor inherit

      hover & do
        important $ backgroundColor inherit

    ".button-close" ? do
      alignSelf flexStart
      padding (rem (1 / 4)) nil nil nil
      textOverflow overflowClip
      width (rem 1)
      height (rem 1)
      minWidth (rem 1)
      "fill" -: "inherit"
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
      borderColor nord6'
      ".warning" ? borderColor nord3'

    ".spacer" ? flexGrow 1


  (".alert-app-level" <> ".badge") ? do
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

    ".compactsize" & do
      paddingAll (rem (1 / 4))
      fontSize (rem (3 / 4))
      ".alert-message" ? do
        paddingTop nil
        paddingBottom nil
      ".button-close" ? paddingAll nil
      ".icon" ? top nil

    ".alert-message" ? do
      flexGrow 1

  (".alert" <> ".tag") ? do
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

  ".alert-light" ? do
    fontColor nord1'
    paddingAll nil
    ".success" & do
      "fill" -: showColor (statusColor Success)

    ".warning" & do
      "fill" -: showColor (darken (2 / 10) (statusColor Warning))

    ".danger" & do
      "fill" -: showColor (statusColor Danger)

    ".info" & do
      "fill" -: showColor (statusColor Info)

    ".compactsize" & ".alert-message" ? do
      fontSize (rem (3 / 4))


alertAppLevel
  :: (PostBuild t m, DomBuilder t m)
  => Status
  -> Dynamic t Text
  -> m a
  -> m (a, Event t ())
alertAppLevel status msg actions = elClass "div" classStr $ do
  elClass "span" "spacer" blank
  alertContent status msg
  result <- actions
  elClass "span" "spacer" blank
  closeEv <- closeBtn def
  pure (result, closeEv)
 where
  classStr = Text.toLower $ Text.unwords $ Prelude.filter
    (Prelude.not . Text.null)
    ["alert-app-level", pack . show $ status]

alertsAppLevel
  :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m, Ord k)
  => (k -> (Text, m ()))
  -> Status
  -> Event t [k]
  -> m ()
alertsAppLevel toMsg cfg addMsg = do
  rec msgs <- foldDyn ($) mempty $ leftmost [appnds <$> addMsg, delete <$> rmEv]
      rmEv <- listViewWithKey msgs $ \_ dynMsg ->
        snd <$> alertAppLevel cfg (fst <$> dynMsg) (dyn_ $ snd <$> dynMsg)

  pure ()
 where
  appnd k m = let msg = toMsg k in Map.insert k msg m
  appnds msgs m = foldr appnd m msgs

  delete toDel m = Map.difference m toDel

alert
  :: (PostBuild t m, DomBuilder t m)
  => AlertConfig
  -> Dynamic t Text
  -> m a
  -> m (a, Event t ())
alert AlertConfig {..} msg actions = elClass "div" classStr $ do
  alertContent _alertConfig_status msg
  result  <- actions
  closeEv <- closeBtn def
  pure (result, closeEv)
 where
  classStr = Text.toLower $ Text.unwords $ Prelude.filter
    (Prelude.not . Text.null)
    [ "alert"
    , pack . show $ _alertConfig_size
    , pack . show $ _alertConfig_status
    ]

alerts
  :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m)
  => AlertConfig
  -> Event t (Text, m ())
  -> m ()
alerts cfg addMsg = do
  rec msgs <- foldDyn ($) (mempty :: Map.Map Integer (Text, m ()))
        $ leftmost [appnd <$> addMsg, delete <$> rmEv]
      rmEv <- listViewWithKey msgs $ \_ dynMsg ->
        snd <$> alert cfg (fst <$> dynMsg) (dyn_ $ snd <$> dynMsg)

  pure ()
 where
  appnd msg m =
    let k = maybe 0 ((+ 1) . fst) (Map.lookupMax m) in Map.insert k msg m

  delete toDel m = Map.difference m toDel

alertLightweight
  :: (PostBuild t m, DomBuilder t m) => AlertConfig -> Dynamic t Text -> m ()
alertLightweight AlertConfig {..} msg = elClass "div" classStr $ do
  alertContent _alertConfig_status msg
 where
  classStr = Text.toLower $ Text.unwords $ Prelude.filter
    (Prelude.not . Text.null)
    [ "alert-light"
    , pack . show $ _alertConfig_size
    , pack . show $ _alertConfig_status
    ]

alertContent
  :: (PostBuild t m, DomBuilder t m) => Status -> Dynamic t Text -> m ()
alertContent status msg = do
  icon def (statusStandardIcon status)
  elClass "span" "alert-message p3" (dynText msg)

