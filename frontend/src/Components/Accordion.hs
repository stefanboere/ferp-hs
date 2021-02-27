{-# LANGUAGE OverloadedStrings #-}
module Components.Accordion
  ( accordion
  , accordionStyle
  )
where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( icon )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Text                      ( Text )
import           Reflex.Dom              hiding ( display
                                                , (&)
                                                )

import           Components.Class
import           Components.Input
import           Components.Icon
import           Nordtheme

accordionStyle :: Css
accordionStyle = ".accordion" ? do
  input # ("type" @= "checkbox") ? display none
  border solid (px 1) grey0'
  borderBottomWidth nil

  ".angle-icon" ? do
    "fill" -: showColor nord3'
    marginRight (rem 0.5)
    transforms [translateY (rem 0.25), rotate (deg 90)]

  label ? do
    cursor pointer
    display block
    padding (rem (1 / 4)) (rem (1 / 2)) (rem (1 / 4)) (rem (1 / 2))

    hover & backgroundColor nord6'

  ".content" ? do
    display none
    backgroundColor white
    borderTop solid (px 1) grey0'
    paddingAll (rem 2)

  input # checked |+ star ? do
    ".angle-icon" ? transforms [translateY (rem 0.25), rotate (deg 180)]
    ".content" ? display flex
    label ? backgroundColor nord4'

  firstOfType & do
    borderTopLeftRadius (px 3) (px 3)
    borderTopRightRadius (px 3) (px 3)

    label & do
      borderTopLeftRadius (px 3) (px 3)
      borderTopRightRadius (px 3) (px 3)


  lastOfType & do
    borderBottomWidth (px 1)
    borderBottomLeftRadius (px 3) (px 3)
    borderBottomRightRadius (px 3) (px 3)

    ".content" ? do
      borderBottomLeftRadius (px 3) (px 3)
      borderBottomRightRadius (px 3) (px 3)


accordion
  :: (MonadIO m, DomBuilder t m, PostBuild t m)
  => Event t Bool
  -> Dynamic t Text
  -> m a
  -> m a
accordion setOpen titl cnt = elClass "section" "accordion" $ do
  idStr <- randomId
  checkboxInputSimple setOpen $ "id" =: idStr
  el "div" $ do
    elAttr "label" ("class" =: "p3" <> "for" =: idStr) $ do
      icon def { _iconConfig_class = Just "angle-icon" } angleIcon
      dynText titl

    elClass "div" "content" cnt
