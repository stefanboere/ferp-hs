{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Components.Navigation
  ( HeaderConfig(..)
  , NavigationPattern(..)
  , app
  , appStyle
  , tabs
  -- * Helpers
  , ahref
  , liahref
  , navGroup
  )
where

import           Prelude                 hiding ( rem
                                                , (**)
                                                )
import           Clay                    hiding ( icon )
import qualified Clay.Media                    as Media
import           Control.Monad                  ( when )
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
  tabsStyle
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

ahref :: (PostBuild t m, DomBuilder t m) => Text -> Dynamic t Bool -> m a -> m a
ahref ref activ = elDynAttr
  "a"
  ((\activ' -> "href" =: ref <> if activ' then "class" =: "active" else mempty)
  <$> activ
  )

liahref
  :: (PostBuild t m, DomBuilder t m) => Text -> Dynamic t Bool -> m a -> m a
liahref ref activ = el "li" . ahref ref activ

navGroup :: (PostBuild t m, DomBuilder t m) => m () -> m () -> m ()
navGroup titl cnt = do
  elClass "section" "nav-group" $ do
    elAttr "input" ("id" =: "a" <> "type" =: "checkbox") blank
    el "div" $ do
      elAttr "label" ("for" =: "a") $ do
        el "span" titl
        icon
          def { _iconConfig_size = 0.7, _iconConfig_class = Just "angle-icon" }
          angleIcon

      el "ul" cnt

app
  :: (PostBuild t m, DomBuilder t m)
  => HeaderConfig t
  -> m ()
  -> m ()
  -> m ()
  -> m ()
  -> m ()
app cfg primary secondary actions page = do
  appHeader cfg primary actions

  case _headerConfig_navigationPattern cfg of
    Header        -> pure ()
    Subnav        -> primaryNavigation (subNav primary)
    Sidenav       -> primaryNavigation (sideNav primary)
    HeaderSubnav  -> secondaryNavigation (subNav secondary)
    HeaderSidenav -> secondaryNavigation (sideNav secondary)
    SubnavSidenav -> do
      primaryNavigation (subNav primary)
      secondaryNavigation (sideNav secondary)

  elClass "article" "main-content" page

navigationCheckbox :: (PostBuild t m, DomBuilder t m) => Text -> m ()
navigationCheckbox idStr = elAttr
  "input"
  ("type" =: "checkbox" <> "id" =: idStr <> "class" =: "nav-opener")
  blank

primaryNavigation :: (PostBuild t m, DomBuilder t m) => m () -> m ()
primaryNavigation x = navigationCheckbox "nav-primary" >> x

secondaryNavigation :: (PostBuild t m, DomBuilder t m) => m () -> m ()
secondaryNavigation x = navigationCheckbox "nav-secondary" >> x

data NavigationPattern = Header -- ^ Menu items in the main header
                       | Subnav  -- ^ A bar below the main header
                       | Sidenav -- ^ A menu on the left
                       | HeaderSubnav -- ^ The header and a bar below the header.
                       | HeaderSidenav -- ^ The header and a menu on the left
                       | SubnavSidenav  -- ^ A bar below the header and a menu on the left
                       deriving (Eq, Show)

instance Default NavigationPattern where
  def = HeaderSubnav

data HeaderConfig t = HeaderConfig
  { _headerConfig_appname :: Dynamic t Text
  , _headerConfig_navigationPattern :: NavigationPattern
  }

instance Reflex t => Default (HeaderConfig t) where
  def = HeaderConfig { _headerConfig_appname           = constDyn ""
                     , _headerConfig_navigationPattern = def
                     }

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

    nav ** a ? headerIconStyle
    ".icon" ? do
      "fill" -: showColor nord4'

commonAppHeaderStyle :: Css
commonAppHeaderStyle = do
  ".app-header" ? do
    zIndex 2
    "grid-area" -: "header"
    display flex
    background nord0'
    fontColor nord6'

    ".hamburger" ? do
      cursor pointer
      display none

    ".icon" ? do
      verticalAlign middle

    ".header-actions" ** a # after ? do
      headerSeparatorStyle
      right nil

    "nav" ? do
      flexDirection row

      a ? do
        position relative

    (star # Clay.not nav ** a) <> label ? headerIconStyle

  ".header-actions" ? do
    display flex
    justifyContent flexEnd
    flexGrow 1

  ".app-logo" ? do
    minWidth (rem 12)

    a # hover ? do
      important $ backgroundColor inherit

    ".icon" ? do
      marginRight (rem 0.5)

headerIconStyle :: Css
headerIconStyle = do
  "fill" -: showColor nord4'
  display inlineFlex
  height (rem 3)
  alignItems center
  fontColor nord4'
  fontSize (rem 1.1)
  textDecoration none
  padding nil (rem 1.2) nil (rem 1.2)
  position relative

  ".icon" ? do
    important $ width (rem 1.5)
    important $ height (rem 1.5)

  hover Clay.& do
    background nord2'
    fontColor nord6'
    "fill" -: showColor nord6'

headerSeparatorStyle :: Css
headerSeparatorStyle = do
  content (stringContent "")
  position absolute
  height (rem 2)
  width (px 1)
  top (rem 0.5)
  backgroundColor nord4'
  opacity 0.15



appHeader
  :: (PostBuild t m, DomBuilder t m) => HeaderConfig t -> m () -> m () -> m ()
appHeader HeaderConfig {..} primary actions = do
  elClass "header" "app-header" $ do
    elAttr "label" ("for" =: "nav-primary" <> "class" =: "hamburger") $ do
      icon def { _iconConfig_size = 1.5 } barsIcon
    elClass "div" "app-logo" $ do
      elAttr "a" ("href" =: "/") $ do
        icon def { _iconConfig_size = 2 } ferpIcon
        dynText _headerConfig_appname

    when
        (      _headerConfig_navigationPattern
        `elem` [Header, HeaderSubnav, HeaderSidenav]
        )
      $ primaryNavigation
      $ elClass "nav" "main-nav" primary

    elClass "div" "header-actions" $ do
      actions

      when
          (      _headerConfig_navigationPattern
          `elem` [HeaderSubnav, HeaderSidenav, SubnavSidenav]
          )
        $ elAttr "label" ("for" =: "nav-secondary" <> "class" =: "hamburger")
        $ do
            icon def { _iconConfig_size = 1.5 } ellipsisVerticalIcon

flexRowLeft :: Css
flexRowLeft = do
  display flex
  flexDirection row
  justifyContent flexStart
  alignItems baseline

tabLinkStyle :: Css
tabLinkStyle = do
  textDecoration none
  display inlineBlock
  fontColor nord3'
  padding nil (rem 0.2) nil (rem 0.2)

  hover Clay.& do
    borderBottom solid 3 nord10'

subNavStyle :: Css
subNavStyle = ".subnav" ? do
  "grid-area" -: "subnav"
  flexRowLeft
  background white
  borderBottom solid 1 grey0'
  paddingLeft (rem 1)

  a ? do
    marginLeft (rem 1)
    marginRight (rem 1)
    tabLinkStyle

  ".active" ? do
    fontColor nord0'
    borderBottom solid 3 nord10'

subNav :: (DomBuilder t m) => m () -> m ()
subNav = elClass "nav" "subnav"

tabsStyle :: Css
tabsStyle = ".tabs" ? do
  flexRowLeft
  borderBottom solid 1 grey0'
  paddingLeft (rem 0)
  lineHeight (rem 2)

  li ? do
    display block
    marginLeft (rem 1)
    marginRight (rem 1)
    firstOfType Clay.& marginLeft (rem 0)
    lastOfType Clay.& marginRight (rem 0)

  a ? tabLinkStyle

  ".active" ? do
    fontColor nord0'
    borderBottom solid 3 nord10'

tabs :: DomBuilder t m => m () -> m ()
tabs = elClass "ul" "tabs"

mobileNavStyle :: Css
mobileNavStyle = do
  ".nav-opener" # checked ? do
    display block
    position absolute
    cursor cursorDefault
    left (rem (-2))
    top (rem 3)
    marginAll nil

    before Clay.& do
      zIndex 1
      absoluteBlock
      important $ backgroundColor nord0'
      important $ borderColor nord0'
      opacity 0.5
      height (vh 110)
      width (vw 110)
      borderWidth nil

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
    fontColor nord3'

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
    paddingRight (rem 0.6)

  nav ? do
    display flex
    flexDirection column
    overflowX hidden
    overflowY auto

    ".icon" ? do
      "fill" -: showColor nord3'
      paddingRight (rem 0.6)

    a ? do
      fontColor inherit
      textDecoration none
      paddingLeft (rem 0.6)
      paddingRight (rem 0.6)

    ul ? do
      display none
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
      paddingRight nil
      ".angle-icon" ? do
        marginLeft (rem (-0.8))
        marginRight (rem 0.2)
        transforms [translate (rem 0.7) (rem 0.7), rotate (deg 90)]

      label ? do
        paddingRight (rem 0.6)

      input # checked |+ star ? do
        ".angle-icon" ? transforms [translateY (rem 0.7), rotate (deg 180)]
        ul ? display flex

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

      label ? do
        justifyContent flexStart
        lineHeight (rem 1.5)
        marginLeft (rem (-0.8))
        ".angle-icon" ? order (-1)


sideNav :: DomBuilder t m => m () -> m ()
sideNav = elClass "nav" "sidenav"

