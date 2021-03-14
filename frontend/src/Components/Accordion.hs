{-# LANGUAGE OverloadedStrings #-}
module Components.Accordion
  ( accordion
  , accordion'
  , accordionEmpty
  , accordionStyle
  , stackview
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
accordionStyle = mconcat [accordionStyle', stackviewStyle]

accordionStyle' :: Css
accordionStyle' = ".accordion" ? do
  input # ("type" @= "checkbox") ? display none
  border solid (px 1) grey0'
  borderBottomWidth nil

  ".angle-icon" ? do
    marginRight (rem 0.5)
    transforms [translateY (rem 0.25), rotate (deg 90)]

  label ? do
    display flex
    padding (rem (1 / 4)) (rem (1 / 2)) (rem (1 / 4)) (rem (1 / 2))

    cursor pointer
    hover & backgroundColor nord6'

  ".content" ? do
    display none
    backgroundColor white
    borderTop solid (px 1) grey0'
    paddingAll (rem 2)

  input # checked |+ star ? do
    ".angle-icon" ? transforms [translateY (rem 0.25), rotate (deg 180)]
    ".content" ? do
      display flex
      flexDirection column
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

  ".empty" & do
    paddingLeft (rem (3 / 2))
    label ? do
      cursor cursorDefault
      hover & backgroundColor inherit

stackviewStyle :: Css
stackviewStyle = do
  ".stack-view" ? do
    flexGrow 1
    display inlineGrid
    "grid-template-columns" -: "1fr 1fr"
    input ? do
      paddingBottom nil
      marginTop (rem (-1 / 4))
    ".helptext" ? marginAll nil

  ".content" |> ".stack-view" # firstOfType ? marginTop (rem (-2))
  ".content" |> ".stack-view" ? do
    padding (rem (1 / 2)) (rem (1 / 2)) (rem (1 / 2)) (rem 2)
    marginLeft (rem (-2))
    marginRight (rem (-2))
    borderBottom solid (px 1) grey0'
  ".content" |> ".stack-view" # lastOfType ? do
    marginBottom (rem (-2))
    borderBottomWidth nil

accordion'
  :: (MonadIO m, DomBuilder t m, PostBuild t m)
  => Event t Bool
  -> m b
  -> m a
  -> m (b, a)
accordion' setOpen titl cnt = elClass "section" "accordion" $ do
  idStr <- randomId
  checkboxInputSimple setOpen $ "id" =: idStr
  el "div" $ do
    l <- elAttr "label" ("class" =: "p3" <> "for" =: idStr) $ do
      icon def { _iconConfig_class = Just "angle-icon" } angleIcon
      titl

    c <- elClass "div" "content" cnt
    pure (l, c)

accordion
  :: (MonadIO m, DomBuilder t m, PostBuild t m)
  => Event t Bool
  -> Dynamic t Text
  -> m a
  -> m a
accordion setOpen titl = fmap snd . accordion' setOpen (dynText titl)

accordionEmpty :: (DomBuilder t m) => m b -> m b
accordionEmpty titl = elClass "section" "accordion empty" $ do
  el "div" $ elAttr "label" ("class" =: "p3") titl

stackview :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
stackview titl cnt = elClass "div" "stack-view" $ do
  dynText titl
  elClass "div" "stack-content" cnt
