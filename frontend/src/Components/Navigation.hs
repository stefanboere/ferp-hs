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
  commonAppHeaderStyle
  commonNavStyle
  query Clay.all [Media.maxWidth 768]
    $ mconcat [mobileNavStyle, mobileHeaderStyle]
  query Clay.all [Media.minWidth 768]
    $ mconcat [subNavStyle, sideNavStyle, appHeaderStyle]

  (html <> body) ? do
    marginAll nil
    maxHeight (vh 100)
    height (vh 100)
    overflowX hidden
    overflowY hidden

  ".nav-opener" ? display none

  ".main-content" ? do
    "grid-area" -: "content"
    paddingAll (rem 1)
    overflowX hidden
    overflowY auto

  body ? do
    display grid
    "grid-template-columns" -: "12rem auto"
    "grid-template-rows" -: "min-content min-content auto"
    boxSizing borderBox
    background white0'
    fontColor nord3'
    fontFamily ["Fira Sans", "Helvetica"] [sansSerif]

tshow :: String -> Text
tshow = pack . show


app :: (PostBuild t m, DomBuilder t m) => HeaderConfig t -> m () -> m ()
app cfg page = do
  appHeader cfg
  elAttr
    "input"
    ("type" =: "checkbox" <> "id" =: "nav-primary" <> "class" =: "nav-opener")
    blank
  subNav
  elAttr
    "input"
    ("type" =: "checkbox" <> "id" =: "nav-secondary" <> "class" =: "nav-opener")
    blank
  sideNav
  elClass "article" "main-content" page

newtype HeaderConfig t = HeaderConfig
  { _headerConfig_appname :: Dynamic t Text
  }

instance Reflex t => Default (HeaderConfig t) where
  def = HeaderConfig { _headerConfig_appname = constDyn "" }


mobileHeaderStyle :: Css
mobileHeaderStyle = do
  body ? do
    "grid-template-areas" -: Text.unlines
      (fmap tshow ["header  header", "subnav subnav", "content content"])
  ".app-header" ? ".hamburger" ? display inlineFlex

  ".app-logo" ? ".icon" ? important (display none)

appHeaderStyle :: Css
appHeaderStyle = do
  body ? do
    "grid-template-areas" -: Text.unlines
      (fmap tshow ["header  header", "subnav subnav", "sidenav content"])
  ".app-header" ? do
    nav ** a # firstOfType # before ? do
      headerSeparatorStyle
      left nil

commonAppHeaderStyle :: Css
commonAppHeaderStyle = do
  ".app-header" ? do
    zIndex 2
    "grid-area" -: "header"
    display flex
    background nord0'
    fontColor nord6'

    ".hamburger" ? display none

    ".icon" ? do
      verticalAlign middle

    ".header-actions" ** a # after ? do
      headerSeparatorStyle
      right nil

    "nav" ? do
      flexDirection row
      width (pct 100)

      a ? do
        position relative

    a <> label ? do
      "fill" -: showColor nord4'
      display inlineFlex
      height (rem 3)
      alignItems center
      fontColor nord4'
      fontSize (rem 1.2)
      textDecoration none
      padding nil (rem 1.2) nil (rem 1.2)
      position relative

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

headerSeparatorStyle :: Css
headerSeparatorStyle = do
  content (stringContent "")
  position absolute
  height (rem 2)
  width (px 1)
  top (rem 0.5)
  backgroundColor nord4'
  opacity 0.15



appHeader :: (PostBuild t m, DomBuilder t m) => HeaderConfig t -> m ()
appHeader HeaderConfig {..} = do
  elClass "header" "app-header" $ do
    elAttr "label" ("for" =: "nav-primary" <> "class" =: "hamburger") $ do
      icon def { _iconConfig_size = 1.5 } barsIcon
    elClass "div" "app-logo" $ do
      elAttr "a" ("href" =: "/") $ do
        icon def { _iconConfig_size = 2 } ferpIcon
        dynText _headerConfig_appname

    elClass "nav" "main-nav" $ do
      elAttr "a" ("href" =: "#") $ do
        icon def { _iconConfig_size = 1.5 } userIcon

    elClass "div" "header-actions" $ do
      elAttr "a" ("href" =: "#") $ do
        icon def { _iconConfig_size = 1.5 } userIcon

      elAttr "a" ("href" =: "#") $ do
        icon def { _iconConfig_size = 1.5 } cogIcon

      elAttr "label" ("for" =: "nav-secondary" <> "class" =: "hamburger") $ do
        icon def { _iconConfig_size = 1.5 } ellipsisVerticalIcon


subNavStyle :: Css
subNavStyle = ".subnav" ? do
  "grid-area" -: "subnav"
  display flex
  flexDirection row
  justifyContent flexStart
  alignItems baseline
  background white
  borderBottom solid 1 grey0'
  paddingLeft (rem 1)

  a ? do
    marginLeft (rem 1)
    marginRight (rem 1)
    display inlineBlock
    fontColor nord3'
    padding nil (rem 0.2) nil (rem 0.2)

    hover Clay.& do
      borderBottom solid 3 nord10'

  ".active" ? do
    fontColor nord0'
    borderBottom solid 3 nord10'

subNav :: (PostBuild t m, DomBuilder t m) => m ()
subNav = elClass "nav" "subnav" $ do
  elAttr "a" ("href" =: "#" <> "class" =: "active") $ text "Subnav link 1"
  elAttr "a" ("href" =: "#") $ text "Subnav link 2"

mobileNavStyle :: Css
mobileNavStyle = do
  ".nav-opener" # checked ? do
    display block
    position absolute
    cursor cursorDefault

    before Clay.& do
      zIndex 1
      absoluteBlock
      important $ backgroundColor nord0'
      important $ borderColor nord0'
      opacity 0.5
      left (rem (-1))
      height (vh 110)
      width (vw 110)

  "#nav-secondary" # checked |+ nav ? do
    right nil

  ".nav-opener" # checked |+ nav ? do
    display block
    position absolute
    top (rem 3)
    zIndex 2
    paddingTop (rem 1)
    minWidth (rem 15)
    height (pct 100 @-@ rem 4)

  nav ? do
    display none
    backgroundColor nord4'

    ".nav-group" ? do
      paddingLeft nil
      label ? do
        paddingLeft (rem 0.6)
        hover Clay.& do
          backgroundColor grey0'

      ".angle-icon" ? do
        marginAll (rem 0.2)
        important $ width (rem 1)
        important $ height (rem 1)

    a ? do
      ".active" Clay.& do
        backgroundColor white0'

      hover Clay.& do
        backgroundColor grey0'

commonNavStyle :: Css
commonNavStyle = do
  nav |> star ? do
    paddingLeft (rem 0.6)

  nav ? do
    display flex
    flexDirection column
    overflowX hidden
    overflowY auto

    a ? do
      fontColor inherit
      textDecoration none
      paddingLeft (rem 0.6)

    ul ? do
      display flex
      flexDirection column
      marginAll nil
      paddingAll nil

    li ? do
      display block

    a ? do
      display block
      lineHeight (rem 2)

    label ? do
      display flex
      justifyContent spaceBetween
      cursor pointer
      lineHeight (rem 2)

    (Clay.span <> a) ? do
      overflow hidden
      whiteSpace nowrap
      textOverflow overflowEllipsis

    input # ("type" @= "checkbox") ? do
      display none

    ".nav-group" ? do
      ".angle-icon" ? do
        marginLeft (rem (-0.8))
        marginRight (rem 0.2)
        "fill" -: showColor nord3'
        transforms [translateY (rem 0.7), rotate (deg 180)]


      input # checked |+ star ? do
        ".angle-icon"
          ? transforms [translate (rem 0.3) (rem 0.4), rotate (deg 90)]
        ul ? display none

sideNavStyle :: Css
sideNavStyle = do
  ".sidenav" |> star ? do
    margin (rem 1.2) nil nil (rem 1.5)
    fontWeight (weight 500)
    lineHeight (rem 1.5)

  ".sidenav" ? do
    "grid-area" -: "sidenav"
    flexDirection column
    fontSize (rem 0.9)
    borderRight solid 1 grey0'

    a ? do
      borderRadius (rem 0.15) nil nil (rem 0.15)
      hover Clay.& do
        background nord6'

      ".active" Clay.& do
        background nord4'

    ".nav-group" ? do
      a ? do
        fontWeight normal
        lineHeight (rem 1.5)

      ".angle-icon" ? do
        marginLeft (rem (-0.8))
        marginRight (rem 0.2)

      label ? do
        lineHeight (rem 1.5)
        ".angle-icon" ? order (-1)

sideNav :: (PostBuild t m, DomBuilder t m) => m ()
sideNav = elClass "nav" "sidenav" $ do
  elAttr "a" ("href" =: "#" <> "class" =: "active") $ text "Subnav link 1"
  elAttr "a" ("href" =: "#") $ text "Subnav link 1 very long link idnee"

  elClass "section" "nav-group" $ do
    elAttr "input" ("id" =: "a" <> "type" =: "checkbox") blank
    el "div" $ do
      elAttr "label" ("for" =: "a") $ do
        el "span" $ text "Collapsible Nav element"
        icon
          def { _iconConfig_size = 0.7, _iconConfig_class = Just "angle-icon" }
          angleIcon

      el "ul" $ do
        el "li" $ elAttr "a" ("href" =: "#") $ text "Link 1"
        el "li" $ elAttr "a" ("href" =: "#") $ text "Link 2"
        el "li" $ elAttr "a" ("href" =: "#" <> "class" =: "active") $ text
          "Link 2 very long link indeed"

