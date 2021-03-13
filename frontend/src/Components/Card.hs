{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Components.Card
  ( card
  , clickableCard
  , cardStyle
  , cardHeader
  , cardImg
  , cardFooter
  , cardContent
  , cardAction
  , ModalSize(..)
  , modal
  , popup
  , modalCloseBtn
  )
where

import           Prelude                 hiding ( rem )

import           Clay
import           Control.Monad.Fix              ( MonadFix )
import           Data.Default
import           Data.Text                      ( Text )
import           Reflex.Dom              hiding ( display
                                                , (&)
                                                )

import           Components.Button
import           Components.Class
import           Components.Icon
import           Nordtheme

cardStyle :: Css
cardStyle = cardStyle' <> clickableCardStyle <> gridStyle <> modalStyle

gridStyle :: Css
gridStyle = ".grid" ? do
  display grid
  alignItems flexStart
  "grid-gap" -: "1rem"
  "grid-template-columns" -: "repeat(auto-fit, minmax(15rem, 1fr))"

cardStyle' :: Css
cardStyle' = ".card" ? do
  border solid (px 1) grey0'
  borderRadiusAll (px 3)
  backgroundColor white
  marginTop (rem (3 / 2))
  boxShadow . pure $ bsColor grey0' $ shadowWithBlur nil (rem (1 / 8)) nil
  overflow hidden
  fontSize (rem (7 / 8))

  star # Clay.not ".card-nopadding" <? do
    paddingAll (rem (3 / 4))
    borderTop solid (px 1) grey0'

    firstChild & borderTopWidth nil

  ".card-footer" ? do
    borderTop solid (px 1) grey0'
    (".primary" <> ".secondary") ? do
      marginTop (rem (3 / 4))
      marginBottom (rem (3 / 4))

  ".card-img" |+ star ? borderTopWidth nil

  ".card-img" Clay.** img ? do
    maxWidth (pct 100)
    width (pct 100)
    height auto
    display block

  ".card-header" ? marginTop nil

  ".card-content" <? (h1 <> h2 <> h3 <> h4 <> h5 <> h6 <> p) # firstOfType ? do
    marginTop nil
    marginBottom (rem (1 / 2))

  ".card-content" <? ul ? do
    marginTop nil
    marginBottom nil

modalStyle :: Css
modalStyle = do
  ".modal" ? do
    position fixed
    zIndex 2
    left nil
    top nil
    width (pct 100)
    height (pct 100)
    overflow auto
    backgroundColor (setA 0.5 nord0')
    display flex
    justifyContent center
    flexDirection column

    ".button-close" ? do
      float floatRight
      marginRight nil
      paddingRight nil
      paddingLeft nil

    ".card-footer" ? do
      Clay.display flex
      justifyContent flexEnd
      alignItems baseline
      borderTopWidth nil
      Clay.button ? do
        minWidth (rem 4)
        justifyContent center

    ".card-content" ? do
      borderTopWidth nil

  ".modal" |> star ? do
    paddingAll (rem (3 / 4))
    "margin" -: "1rem auto"
    maxWidth (pct 100 @-@ rem (7 / 2))

  ".modal" # ".small" |> star ? do
    width (rem 18)

  ".modal" # ".medium" |> star ? do
    width (rem 36)

  ".modal" # ".large" |> star ? do
    width (rem 54)

  ".modal" # ".xtralarge" |> star ? do
    width (rem 72)


clickableCardStyle :: Css
clickableCardStyle = ".card-clickable" ? do
  display block
  textDecoration none
  color inherit
  transitionDuration 0.1
  transitionTimingFunction ease

  hover & do
    transform $ translateY (rem (-1 / 8))
    borderColor hoverColor
    boxShadow . pure $ bsColor hoverColor $ shadowWithBlur nil (rem (1 / 8)) nil
  where hoverColor = rgb 115 151 186

card :: DomBuilder t m => m a -> m a
card = elClass "div" "card"

clickableCard :: DomBuilder t m => Text -> m a -> m a
clickableCard href' =
  elAttr "a" ("class" =: "card card-clickable" <> "href" =: href')

cardHeader :: DomBuilder t m => m a -> m a
cardHeader = elClass "h3" "card-header"

cardImg :: DomBuilder t m => m a -> m a
cardImg = elClass "div" "card-img card-nopadding"

cardContent :: DomBuilder t m => m a -> m a
cardContent = elClass "div" "card-content"

cardFooter :: DomBuilder t m => m a -> m a
cardFooter = elClass "div" "card-footer card-nopadding"

cardAction
  :: (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m (Event t ())
cardAction = btn def { _buttonConfig_priority = ButtonTertiary } . dynText

data ModalSize = ModalSmall | ModalMedium | ModalLarge | ModalExtraLarge
  deriving (Eq, Ord, Enum, Bounded)

instance Default ModalSize where
  def = ModalMedium

modal
  :: (DomBuilder t m, MonadHold t m, MonadFix m)
  => ModalSize
  -> Event t (m (Event t a))
  -> m (Event t a)
modal sz = popup inModal
 where
  inModal cnt = elClass "div" ("modal " <> sizeCls sz) cnt

  sizeCls ModalSmall      = "small"
  sizeCls ModalMedium     = "medium"
  sizeCls ModalLarge      = "large"
  sizeCls ModalExtraLarge = "xtralarge"

modalCloseBtn :: (PostBuild t m, DomBuilder t m) => m (Event t ())
modalCloseBtn = closeBtn def { _iconConfig_size = 3 / 2 }

popup
  :: (DomBuilder t m, MonadHold t m, MonadFix m)
  => (m (Event t a) -> m (Event t a))
  -> Event t (m (Event t a))
  -> m (Event t a)
popup wrapper openEv = do
  rec x <- switchDyn
        <$> widgetHold (pure never) (leftmost [wrapper <$> openEv, closeEv])
      let closeEv = pure never <$ x
  pure x

