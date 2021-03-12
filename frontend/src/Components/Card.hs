{-# LANGUAGE OverloadedStrings #-}
module Components.Card
  ( card
  , clickableCard
  , cardStyle
  , cardHeader
  , cardImg
  , cardFooter
  , cardContent
  , cardAction
  )
where

import           Prelude                 hiding ( rem )

import           Clay
import           Data.Text                      ( Text )
import           Reflex.Dom              hiding ( display
                                                , (&)
                                                )

import           Components.Button
import           Components.Class
import           Nordtheme

cardStyle :: Css
cardStyle = cardStyle' <> clickableCardStyle <> gridStyle

gridStyle :: Css
gridStyle = do
  ".grid" ? do
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

  ".card-content" <? (h1 <> h2 <> h3 <> h4 <> h5 <> h5) # firstOfType ? do
    marginTop nil
    marginBottom (rem (1 / 2))

  ".card-content" <? ul ? do
    marginTop nil
    marginBottom nil


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
