{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Components.Button
  ( ButtonConfig(..)
  , ButtonPriority(..)
  , ActionState(..)
  , buttonStyle
  , btn
  , btnGroup
  , closeBtn
  , btnDropdown
  , btnOverflow
  , angleIconEl
  , dropdownHeader
  , divider
  , signpost
  , signpost'
  , TooltipPosition(..)
  , tooltip
  ) where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( (&)
                                                , icon
                                                )
import qualified Clay                           ( (&) )
import qualified Clay.Media                    as Media
import           Control.Monad.Fix              ( MonadFix )
import           Data.Char                      ( isLower
                                                , isUpper
                                                )
import           Data.Default
import           Data.Map                      as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
import           Reflex
import           Reflex.Dom              hiding ( button
                                                , rangeInput
                                                , textInput
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

instance Semigroup ActionState where
  x <> ActionAvailable = x
  _ <> x               = x

instance Monoid ActionState where
  mempty = def


data ButtonConfig t = ButtonConfig
  { _buttonConfig_priority :: ButtonPriority
  , _buttonConfig_state    :: Dynamic t ActionState
  , _buttonConfig_class    :: Dynamic t Text
  }

instance Reflex t => Default (ButtonConfig t) where
  def = ButtonConfig { _buttonConfig_priority = def
                     , _buttonConfig_state    = constDyn def
                     , _buttonConfig_class    = constDyn ""
                     }

buttonStyle :: Css
buttonStyle =
  buttonStyle'
    <> btnGroupStyle
    <> dropdownStyle
    <> signpostStyle
    <> tooltipStyle

buttonStyle' :: Css
buttonStyle' =
  (Clay.button <> ".file-upload-label" <> input # ("type" @= "submit")) ? do
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

    ".spinner" ? width (rem 1)

    ".spinner" # before ? do
      width (rem (3 / 4))
      height (rem (3 / 4))
      marginTop (rem (-3 / 4))


    disabled Clay.& do
      cursor notAllowed
      borderColor disabledBg
      fontColor nord0'
      (".spinner" <> ".action-icon") |+ ".icon" ? do
        important $ Clay.display none
      ".primary" Clay.& do
        backgroundColor disabledBg

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
  (e, _) <- elDynAttr'
    "button"
    (mkAttrs <$> _buttonConfig_state <*> _buttonConfig_class)
    (dyn (stateIcon <$> _buttonConfig_state) >> lbl)
  pure
    $ gate (current ((== ActionAvailable) <$> _buttonConfig_state))
    $ domEvent Click e
 where
  classStr cls = Text.toLower $ Text.unwords $ Prelude.filter
    (Prelude.not . Text.null)
    [pack $ prioClass _buttonConfig_priority, cls]
  prioClass (ButtonPrimary x) = "primary " <> show x
  prioClass ButtonSecondary   = "secondary"
  prioClass ButtonTertiary    = "tertiary"

  stateIcon ActionError = icon
    def { _iconConfig_status = constDyn (Just Danger)
        , _iconConfig_class  = constDyn $ Just "action-icon"
        }
    errorStandardIcon
  stateIcon ActionSuccess = icon
    def { _iconConfig_status = constDyn (Just Success)
        , _iconConfig_class  = constDyn $ Just "action-icon"
        }
    checkIcon
  stateIcon ActionLoading = spinner def ""
  stateIcon _             = pure ()

  mkAttrs state cls = Map.fromList $ catMaybes
    [ Just ("type", "button")
    , Just ("class", classStr cls)
    , if state == ActionAvailable then Nothing else Just ("disabled", "")
    ]

btnGroupStyle :: Css
btnGroupStyle = ".button-group" ? do
  Clay.display inlineFlex
  alignItems center
  verticalAlign middle
  marginRight (rem (3 / 4))

  (star # firstChild <> star # firstChild |> button) <? do
    borderTopLeftRadius (px 3) (px 3)
    borderBottomLeftRadius (px 3) (px 3)

  (star # lastChild <> star # lastChild |> button) <? do
    borderTopRightRadius (px 3) (px 3)
    borderBottomRightRadius (px 3) (px 3)

  (star <> star |> button) <? do
    marginRight nil
    borderRadiusAll nil

  ".primary" <? marginRight (px 1)

  ".secondary" <? marginRight (px (-1))

  ".secondary" # hover |+ star ? borderLeftColor (rgb 115 151 186)

btnGroup :: DomBuilder t m => m a -> m a
btnGroup = elClass "div" "button-group"

closeBtn :: (PostBuild t m, DomBuilder t m) => IconConfig t -> m (Event t ())
closeBtn cfg =
  btn def { _buttonConfig_priority = ButtonTertiary
          , _buttonConfig_class    = "button-close"
          }
    $ icon cfg timesIcon

dropdownStyle :: Css
dropdownStyle = do
  -- Stacking layout on mobile screens
  query Clay.all [Media.maxWidth 544] $ do
    ".dropdown-menu" ? ".dropdown-menu" ? do
      left (pct 100 @-@ rem (1 / 2))
      right inherit

    ".dropdown-menu" ? ".dropdown-menu" ? ".dropdown-menu" ? do
      right (pct 100 @-@ rem (1 / 2))
      left inherit

  ".divider" ? do
    width (pct 100)
    borderTop solid (px 1) grey0'
    backgroundColor grey0'
    height nil
    marginTop (rem (1 / 4))
    marginBottom (rem (1 / 4))

  (".dropdown" <> ".signpost" <> ".tooltip") ? do
    Clay.display inlineFlex
    position relative
    ".angle-icon" ? transforms [rotate (deg 180)]

    button # ".open" ? zIndex 5

    button # ".open" # before ? do
      cursor cursorDefault
      content (stringContent "")
      Clay.display block
      position fixed
      width (vw 100)
      height (vh 100)
      top nil
      left nil

  ".dropdown-header" ? do
    marginTop nil
    paddingLeft (rem (1 / 2))

  (".dropdown-menu" <> ".combobox-menu") ? do
    flexDirection column
    minWidth (rem 12)
    top (rem 2)
    boxShadow . pure $ bsColor grey0' $ shadowWithBlur nil
                                                       (rem (1 / 16))
                                                       (rem (1 / 8))

    ".open" Clay.& Clay.display flex

  (".signpost-menu" <> ".dropdown-menu" <> ".tooltip-menu" <> ".combobox-menu")
    ? do
        Clay.display none
        position absolute
        maxWidth (rem 20)
        borderRadiusAll (px 3)
        background white0'
        border solid (px 1) grey0'
        paddingTop (rem (1 / 2))
        paddingBottom (rem (1 / 2))
        zIndex 5

        ".angle-icon" ? transforms [rotate (deg 90)]

        ".dropdown-menu" ? do
          left (pct 100 @-@ rem (1 / 2))
          top nil

  ".dropdown" Clay.** ".dropdown-menu" Clay.** (button <> a) ? do
    Clay.display flex
    justifyContent spaceBetween
    paddingLeft (rem 1)
    height (rem (3 / 2))
    width (pct 100)
    textTransform none
    marginAll nil
    borderRadiusAll nil
    backgroundColor inherit
    color nord3'
    "fill" -: showColor nord3'
    fontWeight (weight 400)

    hover Clay.& Clay.not (star # disabled) Clay.& background nord6'

    disabled Clay.& fontColor grey0'

    ".open" Clay.& do
      background nord4'
      hover Clay.& backgroundColor nord4'

      before Clay.& Clay.display none

btnDropdown
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => ButtonConfig t
  -> m a
  -> m (Event t b)
  -> m (Event t b)
btnDropdown cfg titl cnt =
  snd <$> btnDropdown' "dropdown" cfg (titl >> angleIconEl) (((), ) <$> cnt)

angleIconEl :: (DomBuilder t m, PostBuild t m) => m ()
angleIconEl =
  icon def { _iconConfig_class = constDyn $ Just "angle-icon" } angleIcon

btnOverflow
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => ButtonConfig t
  -> m (Event t b)
  -> m (Event t b)
btnOverflow cfg cnt = snd <$> btnDropdown' "dropdown"
                                           cfg
                                           (icon def ellipsisHorizontalIcon)
                                           (((), ) <$> cnt)

btnDropdown'
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => Text
  -> ButtonConfig t
  -> m a
  -> m (x, Event t b)
  -> m (x, Event t b)
btnDropdown' typeStr' cfg titl cnt = elClass "div" typeStr $ do
  rec clickEv <- btn
        cfg
          { _buttonConfig_class = mkCls' <$> openDyn <*> _buttonConfig_class cfg
          }
        titl

      (x, actionEv) <- elDynClass
        "div"
        (((typeStr <> "-menu" <> typeStrOther <> " ") <>) . mkCls <$> openDyn)
        cnt

      openDyn <- foldDyn ($) False
        $ leftmost [Prelude.not <$ clickEv, const False <$ actionEv]
  pure (x, actionEv)

 where
  (typeStr, typeStrOther) = Text.span (/= ' ') typeStr'
  mkCls' True  x = "open " <> x
  mkCls' False x = x
  mkCls x = mkCls' x ""

dropdownHeader :: (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m ()
dropdownHeader = elClass "h4" "dropdown-header" . dynText

divider :: DomBuilder t m => m ()
divider = elAttr "div" ("class" =: "divider" <> "role" =: "separator") blank

signpostStyle :: Css
signpostStyle = do
  ".signpost-menu" ? do
    minWidth (rem 15)
    minHeight (rem 5)
    maxHeight (rem 30)
    paddingAll (rem 1)

    ".button-close" ? do
      position absolute
      right (rem (1 / 4))
      top (rem (1 / 4))

    ".button-close" |+ star ? marginTop nil

  ".signpost-menu" <> ".tooltip-menu" ? do
    ".open" Clay.& Clay.display block

    before Clay.& do
      content $ stringContent ""
      Clay.display block
      position absolute
      width (rem (1 / 2))
      height (rem (1 / 2))
      backgroundColor inherit

  (".signpost" <> ".tooltip") ? do
    button # ".tertiary" ? do
      paddingLeft (rem (1 / 4))
      paddingRight (rem (1 / 4))
      marginAll nil

    button # ".open" ? do
      color (rgb 115 151 186)
      "fill" -: showColor (rgb 115 151 186)

  ".top-left" ? do
    bottom (pct 100 @+@ rem (1 / 2))
    right (pct 50)
    borderBottomRightRadius nil nil
    before Clay.& right (px (-1))

  (".top-left" <> ".top-middle") ? do
    before Clay.& do
      bottom (rem (-1 / 4) @-@ px 2)
      borderRight solid (px 1) grey0'
      borderBottom solid (px 1) grey0'
      transforms [skewY (deg 45)]

  ".top-middle" ? bottom (pct 100 @+@ rem (1 / 2))

  (".top-middle" <> ".bottom-middle") ? do
    right (pct 50)
    transforms [translate (pct 50) nil]
    before Clay.& right (pct 50)

  ".top-right" ? do
    bottom (pct 100 @+@ rem (1 / 2))
    left (pct 50)
    borderBottomLeftRadius nil nil
    before Clay.& do
      left (px (-1))
      bottom (rem (-1 / 4) @-@ px 2)
      borderLeft solid (px 1) grey0'
      borderBottom solid (px 1) grey0'
      transforms [skewY (deg (-45))]

  ".right-top" ? do
    bottom (pct 50)
    left (pct 100 @+@ rem (1 / 2))
    borderBottomLeftRadius nil nil
    before Clay.& do
      bottom (px (-1))
      left (rem (-1 / 4) @-@ px 1)
      borderLeft solid (px 1) grey0'
      borderBottom solid (px 1) grey0'
      transforms [skewX (deg (-45))]

  ".right-middle" ? left (pct 100 @+@ rem (1 / 2))

  ".right-middle" <> ".left-middle" ? do
    top (pct 50)
    transforms [translate nil (pct (-50))]
    before Clay.& top (pct 50)

  (".right-middle" <> ".right-bottom") # before ? do
    left (rem (-1 / 4) @-@ px 1)
    borderLeft solid (px 1) grey0'
    borderTop solid (px 1) grey0'
    transforms [skewX (deg 45)]

  ".right-bottom" ? do
    top (pct 50)
    left (pct 100 @+@ rem (1 / 2))
    borderTopLeftRadius nil nil
    before Clay.& top (px (-1))

  ".bottom-right" ? do
    top (pct 100 @+@ rem (1 / 2))
    left (pct 50)
    borderTopLeftRadius nil nil
    before Clay.& do
      left (px (-1))
      top (rem (-1 / 4) @-@ px 2)
      borderLeft solid (px 1) grey0'
      borderTop solid (px 1) grey0'
      transforms [skewY (deg 45)]

  ".bottom-middle" ? top (pct 100 @+@ rem (1 / 2))

  (".bottom-middle" <> ".bottom-left") # before ? do
    top (rem (-1 / 4) @-@ px 2)
    borderRight solid (px 1) grey0'
    borderTop solid (px 1) grey0'
    transforms [skewY (deg (-45))]

  ".bottom-left" ? do
    top (pct 100 @+@ rem (1 / 2))
    right (pct 50)
    borderTopRightRadius nil nil
    before Clay.& right (px (-1))

  ".left-bottom" ? do
    top (pct 50)
    right (pct 100 @+@ rem (1 / 2))
    borderTopRightRadius nil nil
    before Clay.& top (px (-1))

  (".left-bottom" <> ".left-middle") # before ? do
    right (rem (-1 / 4) @-@ px 1)
    borderRight solid (px 1) grey0'
    borderTop solid (px 1) grey0'
    transforms [skewX (deg (-45))]

  ".left-middle" ? right (pct 100 @+@ rem (1 / 2))

  ".left-top" ? do
    bottom (pct 50)
    right (pct 100 @+@ rem (1 / 2))
    borderBottomRightRadius nil nil
    before Clay.& do
      bottom (px (-1))
      right (rem (-1 / 4) @-@ px 1)
      borderBottom solid (px 1) grey0'
      borderRight solid (px 1) grey0'
      transforms [skewX (deg 45)]


data TooltipPosition = TopLeft
              | TopMiddle
              | TopRight
              | RightTop
              | RightMiddle
              | RightBottom
              | BottomRight
              | BottomMiddle
              | BottomLeft
              | LeftBottom
              | LeftMiddle
              | LeftTop
              deriving (Eq, Show, Enum, Bounded)

instance Default TooltipPosition where
  def = RightMiddle

signpost
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => TooltipPosition
  -> m ()
  -> m ()
signpost = fmap (fmap fst) . signpost' (icon def infoStandardIcon)

signpost'
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => m ()
  -> TooltipPosition
  -> m a
  -> m (a, Event t ())
signpost' ico pos cnt =
  btnDropdown' ("signpost " <> toSnake (pack (show pos)))
               def { _buttonConfig_priority = ButtonTertiary }
               ico
    $ do
        closeEv <- closeBtn def
        x       <- cnt
        pure (x, closeEv)

toSnake :: Text -> Text
toSnake x =
  let (x1, rest) = Text.span isUpper x
      (x2, x3  ) = Text.span isLower rest
  in  Text.toLower $ x1 <> x2 <> "-" <> x3

tooltipStyle :: Css
tooltipStyle = do
  ".tooltip-menu" ? do
    backgroundColor nord0'
    borderColor nord0'
    before Clay.& borderColor nord0'

    fontColor nord6'
    "width" -: "max-content"
    paddingAll (rem (1 / 2))
    height (rem 1)

  ".tooltip" # hover Clay.** ".tooltip-menu" ? Clay.display block

tooltip
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => TooltipPosition
  -> Dynamic t Text
  -> m ()
tooltip pos cnt =
  (() <$)
    <$> btnDropdown' ("tooltip " <> toSnake (pack (show pos)))
                     def { _buttonConfig_priority = ButtonTertiary }
                     (icon def infoStandardIcon)
    $   dynText cnt
    >>  pure ((), never)

