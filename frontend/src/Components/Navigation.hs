{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Components.Navigation
  ( HeaderConfig(..)
  , app
  , appStyle
  )
where

import           Prelude                 hiding ( rem
                                                , (**)
                                                )

import           Clay                    hiding ( icon )
import qualified Clay.Media                    as Media
import           Data.Default
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
import           Reflex.Dom              hiding ( display )

import           Components.Class
import           Components.Icon
import           Nordtheme


appStyle :: Css
appStyle = do
  appHeaderStyle
  subNavStyle
  sideNavStyle

  (html <> body) ? do
    marginAll nil
    maxHeight (vh 100)
    height (vh 100)

  body ? do
    display grid
    "grid-template-columns" -: "12rem auto"
    "grid-template-rows" -: "min-content min-content auto"
    "grid-template-areas" -: Text.unlines
      (fmap tshow ["header  header", "subnav subnav", "sidenav content"])
    boxSizing borderBox
    background white0'
    fontColor nord3'
    fontFamily ["Fira Sans", "Helvetica"] [sansSerif]

 where
  tshow :: String -> Text
  tshow = pack . show



app :: (PostBuild t m, DomBuilder t m) => HeaderConfig t -> m () -> m ()
app cfg page = do
  appHeader cfg
  subNav
  sideNav
  el "article" page

newtype HeaderConfig t = HeaderConfig
  { _headerConfig_appname :: Dynamic t Text
  }

instance Reflex t => Default (HeaderConfig t) where
  def = HeaderConfig { _headerConfig_appname = constDyn "" }

appHeaderStyle :: Css
appHeaderStyle = do
  ".app-header" ? do
    "grid-area" -: "header"
    display flex
    background nord0'
    fontColor nord6'

    ".hamburger" ? display none

    ".icon" ? do
      verticalAlign middle

    ((".header-actions" ** a # after) <> (nav ** a # firstOfType # before)) ? do
      absoluteBlock
      display inlineBlock
      height (rem 2)
      width (px 1)
      top (rem 0.5)
      left nil
      backgroundColor nord4'
      opacity 0.15


    "nav" ? do
      display flex
      width (pct 100)

      a ? do
        position relative

    a ? do
      "fill" -: showColor nord4'
      display inlineFlex
      height (rem 3)
      alignItems center
      fontColor nord4'
      fontSize (rem 1.2)
      textDecoration none
      padding nil (rem 1.2) nil (rem 1.2)

      hover Clay.& do
        background nord2'
        fontColor nord6'
        "fill" -: showColor nord6'

  ".header-actions" ? do
    display flex
    justifyContent flexEnd
    flexGrow 1

  ".app-logo" ? do
    minWidth (rem 12)

    a # hover ? do
      backgroundColor inherit
      fontColor inherit

    ".icon" ? do
      marginRight (rem 0.5)

  query Clay.all [Media.maxWidth 768] mobileHeaderStyle

mobileHeaderStyle :: Css
mobileHeaderStyle = do
  ".app-header" ? ".hamburger" ? display inlineFlex

  ".app-logo" ? ".icon" ? important (display none)


appHeader :: (PostBuild t m, DomBuilder t m) => HeaderConfig t -> m ()
appHeader HeaderConfig {..} = do
  elClass "header" "app-header" $ do
    elAttr "a" ("href" =: "#" <> "class" =: "hamburger") $ do
      icon def { _iconConfig_size = 1.5 } barsIcon
    elClass "div" "app-logo" $ do
      elAttr "a" ("href" =: "/") $ do
        icon def { _iconConfig_size = 2 } ferpIcon
        dynText _headerConfig_appname

    el "nav" $ do
      elAttr "a" ("href" =: "#") $ do
        icon def { _iconConfig_size = 1.5 } userIcon
      elClass "div" "header-actions" $ do
        elAttr "a" ("href" =: "#") $ do
          icon def { _iconConfig_size = 1.5 } userIcon

        elAttr "a" ("href" =: "#") $ do
          icon def { _iconConfig_size = 1.5 } cogIcon

        elAttr "a" ("href" =: "#" <> "class" =: "hamburger") $ do
          icon def { _iconConfig_size = 1.5 } ellipsisVerticalIcon


subNavStyle :: Css
subNavStyle = ".subnav" ? do
  "grid-area" -: "subnav"
  display flex
  justifyContent spaceBetween
  alignItems center
  background white
  borderBottom solid 1 grey0'

  ul ? do
    marginAll nil
    paddingLeft (rem 1)

  li ? do
    display inlineBlock
    padding nil (rem 1.2) nil (rem 1.2)

  a ? do
    display inlineBlock
    lineHeight (rem 2)
    fontColor nord3'
    textDecoration none
    padding nil (rem 0.2) nil (rem 0.2)

    hover Clay.& do
      borderBottom solid 3 nord10'

  ".active" ? do
    fontColor nord0'
    borderBottom solid 3 nord10'

subNav :: (PostBuild t m, DomBuilder t m) => m ()
subNav = elClass "nav" "subnav" $ do
  el "ul" $ do
    el "li" $ elAttr "a" ("href" =: "#" <> "class" =: "active") $ text
      "Subnav link 1"
    el "li" $ elAttr "a" ("href" =: "#") $ text "Subnav link 2"

sideNavStyle :: Css
sideNavStyle = do
  ".sidenav" |> (a <> section) ? do
    margin (rem 1.2) nil nil (rem 1.5)

  ".sidenav" ? borderRight solid 1 grey0'

  (".verticalnav" <> ".sidenav") ? do
    "grid-area" -: "sidenav"
    display flex
    flexDirection column
    overflow auto
    fontSize (rem 0.9)

    (a <> ".nav-group") ? do
      lineHeight (rem 1.5)
      fontColor inherit
      fontWeight (weight 500)
      paddingLeft (rem 0.6)
      borderRadius (rem 0.15) nil nil (rem 0.15)

    (a <> (".nav-group" ** Clay.span)) ? do
      display inlineBlock
      overflow hidden
      whiteSpace nowrap
      textOverflow overflowEllipsis

    a ? do
      textDecoration none

      hover Clay.& do
        background nord6'

      ".active" Clay.& do
        background nord4'

    ".nav-group" ? do
      ".icon" ? do
        marginLeft (rem (-0.8))
        marginRight (rem 0.2)
        "fill" -: showColor nord3'
        transforms [translateY (rem 0.7), rotate (deg 180)]

      label ? do
        display flex
        fontWeight (weight 500)
        cursor pointer

        Clay.span ? order 1

      input # ("type" @= "checkbox") ? do
        display none

      li ? do
        display block
        a ? do
          display block
          lineHeight (rem 1.5)
          fontWeight normal

      input # checked |+ Clay.div ? do
        ".icon" ? transforms [translate (rem 0.3) (rem 0.4), rotate (deg 90)]
        ul ? display none

      ul ? do
        marginAll nil
        paddingAll nil

  ".verticalnav" ? do
    backgroundColor nord4'

    ".nav-group" ? do
      lineHeight (rem 2)
      paddingLeft nil
      label ? do
        paddingLeft (rem 0.6)
        lineHeight (rem 2)
        hover Clay.& do
          backgroundColor grey0'

      li ? do
        a ? lineHeight (rem 2)

      ".icon" ? do
        order 1
        marginAll (rem 0.2)

    a ? do
      lineHeight (rem 2)

      ".active" Clay.& do
        backgroundColor white0'

      hover Clay.& do
        backgroundColor grey0'

  article ? do
    "grid-area" -: "content"
    paddingAll (rem 1)
    overflowX hidden
    overflowY auto


sideNav :: (PostBuild t m, DomBuilder t m) => m ()
sideNav = verticalSideNav True

verticalSideNav :: (PostBuild t m, DomBuilder t m) => Bool -> m ()
verticalSideNav isSideNav =
  elClass "nav" (if isSideNav then "sidenav" else "verticalnav") $ do
    elAttr "a" ("href" =: "#" <> "class" =: "active") $ text "Subnav link 1"
    elAttr "a" ("href" =: "#") $ text "Subnav link 1 very long link idnee"

    elClass "section" "nav-group" $ do
      elAttr "input" ("id" =: "a" <> "type" =: "checkbox") blank
      el "div" $ do
        elAttr "label" ("for" =: "a")
          $ let sz = if isSideNav then 0.7 else 1
            in  do
                  el "span" $ text "Collapsible Nav element"
                  icon def { _iconConfig_size = sz } angleIcon

        el "ul" $ do
          el "li" $ elAttr "a" ("href" =: "#") $ text "Link 1"
          el "li" $ elAttr "a" ("href" =: "#") $ text "Link 2"
          el "li" $ elAttr "a" ("href" =: "#" <> "class" =: "active") $ text
            "Link 2 very long link indeed"

