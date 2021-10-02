{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Components.Navigation
  ( HeaderConfig(..)
  , NavigationPattern(..)
  , app
  , appStyle
  , tabs
  , tabsVertical
  -- * Helpers
  , navGroup
  , treeview
  , leaf
  , elAttrClick_
  , coerceUri
  , safelink
  , safelinkGroup
  , ahrefPreventDefault
  )
where

import           Clay                    hiding ( icon )
import qualified Clay.Media                    as Media
import qualified Clay.Flexbox                  as Flexbox
import           Control.Monad                  ( when )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.ByteString.Char8         as B
                                                ( pack )
import           Data.Default
import           Data.Map                       ( Map )
import           Data.Monoid                    ( Any(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
import           Prelude                 hiding ( (**)
                                                , rem
                                                )
import           Reflex.Dom              hiding ( display )
import           Servant.API                    ( toUrlPiece )
import qualified Servant.Links                 as L
                                                ( Link
                                                , URI(..)
                                                , linkURI
                                                , uriPath
                                                )
import           URI.ByteString

import           Components.Class
import           Components.Icon
import           Components.Input.Basic         ( checkboxInputSimple
                                                , randomId
                                                )
import           Nordtheme

appStyle :: Css
appStyle = do
  utilsStyle
  typographyStyle
  commonAppHeaderStyle
  commonNavStyle
  tabsStyle
  treeviewStyle
  query Clay.all [Media.maxWidth 768]
    $ mconcat [mobileNavStyle, mobileHeaderStyle, verticalTabsStyleMobile]
  query Clay.all [Media.minWidth 768]
    $ mconcat [subNavStyle, sideNavStyle, appHeaderStyle, verticalTabsStyle]

  (html <> body) ? do
    marginAll nil
    maxHeight (vh 100)
    height (vh 100)

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

utilsStyle :: Css
utilsStyle = do
  ".flex-column" ? do
    display flex
    flexDirection column
    height (pct 100)

typographyStyle :: Css
typographyStyle = do
  ".main-content" ** h1 # firstOfType ? do
    marginTop nil
    marginBottom (rem (1 / 2))

  (h1 <> h2 <> h3 <> h4 <> h5 <> h6) ? do
    marginTop (rem (3 / 2))
    marginBottom nil

  h1 ? do
    fontWeight (weight 250)
    fontSize (rem 2.2)
    marginBottom (rem (2 / 3))

  h2 ? do
    fontWeight (weight 300)
    fontSize (rem (7 / 4))

  h3 ? do
    fontWeight (weight 300)
    fontSize (rem (11 / 8))

  h4 ? do
    fontWeight (weight 350)
    fontSize (rem (9 / 8))

  h5 ? do
    fontWeight (weight 400)
    fontSize (rem 1)

  h6 ? do
    fontWeight (weight 500)
    fontSize (rem (7 / 8))

  ".p2" <> ".form_header" <> ".stackview_header" <> ".treeview_header" ? do
    fontWeight (weight 500)
    fontSize (rem (13 / 16))

  ".p3"
    <> ".alert"
    <> tbody
    <> thead
    <> ".tooltip"
    <> form
    <> ".stackview"
    <> ".treeview"
    ?  fontSize (rem (13 / 16))

  ".p4"
    <> (form ** label)
    <> ".button"
    <> Clay.button
    <> th
    <> ".dropdown-header"
    ?  do
         fontWeight (weight 600)
         fontSize (rem (13 / 16))

  ".p5" <> tfoot <> ".chart" ? fontSize (rem (12 / 16))

  ".p6" <> (".button" <> Clay.button) # ".compactsize" ? do
    fontWeight (weight 600)
    fontSize (rem (11 / 16))

  ".p7" <> ".tag" ? fontSize (rem (11 / 16))

  ".p8" <> ".badge" ? fontSize (rem (10 / 16))

  body ? do
    background white0'
    fontColor nord3'
    "fill" -: showColor nord3'
    fontFamily ["Fira Sans", "Segoe UI"] [sansSerif]

  ul ? do
    marginLeft nil
    marginRight nil
    paddingLeft (rem (3 / 2))

tshow :: String -> Text
tshow = pack . show

elAttrClick_
  :: forall t m
   . (PostBuild t m, DomBuilder t m)
  => Text
  -> Map Text Text
  -> m ()
  -> m (Event t ())
elAttrClick_ elName attrs cnt = do
  (e, _) <- elAttr' elName attrs cnt
  pure $ domEvent Click e

navGroup
  :: (MonadIO m, PostBuild t m, DomBuilder t m)
  => Event t Bool
  -> m ()
  -> m a
  -> m a
navGroup = navGroup' True 0.7 "nav-group"

navGroup'
  :: (MonadIO m, PostBuild t m, DomBuilder t m)
  => Bool
  -> Double
  -> Text
  -> Event t Bool
  -> m ()
  -> m a
  -> m a
navGroup' initC iconSize cls setOpen titl cnt = elClass "section" cls $ do
  idStr <- randomId
  _     <-
    checkboxInputSimple
      initC
      ((if initC then Prelude.not else Prelude.id) <$> setOpen)
    $  "id"
    =: idStr
    <> "style"
    =: "display:none"
  el "div" $ do
    elAttr "label" ("for" =: idStr) $ do
      el "span" titl
      icon
        def { _iconConfig_size  = iconSize
            , _iconConfig_class = constDyn $ Just "angle-icon"
            }
        angleIcon

    el "ul" cnt

ahrefPreventDefault
  :: forall t m
   . (PostBuild t m, DomBuilder t m)
  => Dynamic t Text
  -> Dynamic t Bool
  -> Map Text Text
  -> m ()
  -> m (Event t ())
ahrefPreventDefault ref activ attrs cnt = do
  (e, _) <- elDynAttrEventSpec'
    (addEventSpecFlags (Nothing :: Maybe (DomBuilderSpace m))
                       Click
                       (const preventDefault)
    )
    "a"
    (   (\ref' activ' -> attrs <> "href" =: ref' <> if activ'
          then "class" =: "active"
          else mempty
        )
    <$> ref
    <*> activ
    )
    cnt

  pure $ domEvent Click e

-- elDynAttr' which prevents default
elDynAttrEventSpec'
  :: (DomBuilder t m, PostBuild t m)
  => (  EventSpec (DomBuilderSpace m) EventResult
     -> EventSpec (DomBuilderSpace m) EventResult
     )
  -> Text
  -> Dynamic t (Map Text Text)
  -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynAttrEventSpec' evSpec elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = ElementConfig
        { _elementConfig_namespace         = Nothing
        , _elementConfig_initialAttributes = mempty
        , _elementConfig_modifyAttributes  = Just $ fmapCheap
                                               mapKeysToAttributeName
                                               modifyAttrs
        , _elementConfig_eventSpec         = evSpec def
        }
  result    <- Reflex.Dom.element elementTag cfg child
  postBuild <- getPostBuild
  notReadyUntil postBuild
  return result

safelink
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> L.Link
  -> m ()
  -> m (Dynamic t Bool, Event t L.Link)
safelink dynLoc lnk cnt = do
  closeEv <- ahrefPreventDefault (constDyn ("/" <> toUrlPiece lnk))
                                 isActiveDyn
                                 mempty
                                 cnt
  pure (isActiveDyn, lnk <$ closeEv)
 where
  isActiveDyn = (lUriPath lnk ==) . uriPath <$> dynLoc
  lUriPath    = ("/" <>) . B.pack . L.uriPath . L.linkURI


coerceUri :: L.URI -> URI
coerceUri uri = URI { uriScheme    = scheme (L.uriScheme uri)
                    , uriAuthority = Nothing
                    , uriPath      = "/" <> B.pack (L.uriPath uri)
                    , uriQuery     = parseQuery (L.uriQuery uri)
                    , uriFragment  = parseFragment (L.uriFragment uri)
                    }
 where
  scheme "" = Scheme "http"
  scheme x  = Scheme $ B.pack x
  parseFragment ('#' : xs) = Just $ B.pack xs
  parseFragment _          = Nothing
  parseQuery = Query . fmap toPair . wordsWhen (== '&') . dropWhile (== '?')

  toPair x =
    let (x1, x2) = break (== '=') x
    in  (B.pack x1, B.pack . dropWhile (== '=') $ x2)

  wordsWhen :: (Char -> Bool) -> String -> [String]
  wordsWhen prd str = case dropWhile prd str of
    "" -> []
    s' -> w : wordsWhen prd s'' where (w, s'') = break prd s'


-- | A group of links which automatically opens if one child is active
safelinkGroup
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => m ()
  -> [m (Dynamic t Bool, Event t L.Link)]
  -> m (Event t L.Link)
safelinkGroup lbl childs = do
  rec closeEvs <- navGroup (leftmost [initActive, updated anyActive]) lbl
        $ sequence childs

      postBuild <- getPostBuild
      let anyActive  = fmap getAny $ mconcat $ fmap (fmap Any . fst) closeEvs
      let initActive = tagPromptlyDyn anyActive postBuild
  pure $ leftmost $ fmap snd closeEvs

app
  :: (MonadFix m, PostBuild t m, DomBuilder t m)
  => HeaderConfig t
  -> m (Event t a)
  -> m (Event t a)
  -> m ()
  -> m c
  -> m (Event t a, c)
app cfg primary secondary actions page = do
  evA <- appHeader cfg primary actions

  evB <- case _headerConfig_navigationPattern cfg of
    Header        -> pure never
    Subnav        -> primaryNavigation (subNav primary)
    Sidenav       -> primaryNavigation (sideNav primary)
    HeaderSubnav  -> secondaryNavigation (subNav secondary)
    HeaderSidenav -> secondaryNavigation (sideNav secondary)
    SubnavSidenav -> do
      evBa <- primaryNavigation (subNav primary)
      evBb <- secondaryNavigation (sideNav secondary)
      pure $ leftmost [evBa, evBb]

  pg <- elClass "article" "main-content" page
  pure (leftmost [evA, evB], pg)

navigationCheckbox :: (DomBuilder t m) => Text -> Event t Bool -> m ()
navigationCheckbox idStr setOpen =
  checkboxInputSimple False setOpen ("id" =: idStr <> "class" =: "nav-opener")
    >> pure ()

primaryNavigation
  :: (MonadFix m, PostBuild t m, DomBuilder t m)
  => m (Event t a)
  -> m (Event t a)
primaryNavigation x = do
  rec navigationCheckbox "nav-primary" (False <$ closeEv)
      closeEv <- x
  pure closeEv

secondaryNavigation
  :: (MonadFix m, PostBuild t m, DomBuilder t m)
  => m (Event t a)
  -> m (Event t a)
secondaryNavigation x = do
  rec navigationCheckbox "nav-secondary" (False <$ closeEv)
      closeEv <- x
  pure closeEv

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
  { _headerConfig_appname           :: Dynamic t Text
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
    zIndex 20
    "grid-area" -: "header"
    display flex
    background nord0'
    fontColor nord6'

    ".hamburger" ? do
      cursor pointer
      display none

    ".icon" ? verticalAlign middle

    ".header-actions" ** (a <> ".dropdown") # after ? do
      headerSeparatorStyle
      right nil

    "nav" ? do
      flexDirection row

      a ? position relative

    (star # Clay.not nav ** (a <> Clay.button)) <> label ? headerIconStyle

    Clay.button ? do
      important $ backgroundColor inherit
      important $ marginAll nil
      fontColor nord6'
      "fill" -: showColor nord6'

    ".dropdown-menu" ** (a <> Clay.button) ? do
      fontSize (rem 1)
      important $ height (rem 2)
      important $ width inherit
      hover Clay.& fontColor nord3'

  ".header-actions" ? do
    display flex
    justifyContent flexEnd
    flexGrow 1

    ".dropdown-menu" ? do
      right nil
      top (rem 3)
      left inherit


  ".app-logo" ? do
    minWidth (rem 12)

    a # hover ? do
      important $ backgroundColor inherit

    ".icon" ? marginRight (rem 0.5)

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

  ".icon" # Clay.not ".angle-icon" ? do
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
  :: (MonadFix m, PostBuild t m, DomBuilder t m)
  => HeaderConfig t
  -> m (Event t a)
  -> m ()
  -> m (Event t a)
appHeader HeaderConfig {..} primary actions =
  elClass "header" "app-header" $ do
    elAttr "label" ("for" =: "nav-primary" <> "class" =: "hamburger")
      $ icon def { _iconConfig_size = 1.5 } barsIcon
    elClass "div" "app-logo" $ elAttr "a" ("href" =: "/") $ do
      icon def { _iconConfig_size = 2 } ferpIcon
      dynText _headerConfig_appname

    lnkEv <-
      if _headerConfig_navigationPattern
         `elem` [Header, HeaderSubnav, HeaderSidenav]
      then
        primaryNavigation $ elClass "nav" "main-nav" primary
      else
        pure never

    elClass "div" "header-actions" $ do
      actions

      when
          (      _headerConfig_navigationPattern
          `elem` [HeaderSubnav, HeaderSidenav, SubnavSidenav]
          )
        $ elAttr "label" ("for" =: "nav-secondary" <> "class" =: "hamburger")
        $ icon def { _iconConfig_size = 1.5 } ellipsisVerticalIcon

    pure lnkEv

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

  hover Clay.& borderBottom solid 3 nord10'

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

subNav :: (DomBuilder t m) => m a -> m a
subNav = elClass "nav" "subnav"

tabsStyle :: Css
tabsStyle = ".tabs" ? tabsStyle'

tabsStyle' :: Css
tabsStyle' = do
  flexRowLeft
  borderBottom solid 1 grey0'
  paddingLeft (rem 0)
  lineHeight (rem 2)
  marginTop (rem (1 / 2))
  marginBottom (rem (1 / 2))
  flexWrap Flexbox.wrap

  li ? do
    display block
    height (rem 2)
    marginLeft (rem (1 / 2))
    marginRight (rem (1 / 2))
    boxSizing borderBox
    firstOfType Clay.& marginLeft (rem 0)
    lastOfType Clay.& marginRight (rem 0)
    tabLinkStyle
    cursor pointer

  ".active" ? do
    fontColor nord0'
    borderBottom solid 3 nord10'

tabs
  :: (Ord k, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => Map k (Text, m ())
  -> m ()
tabs = tabDisplay "tabs" "active"

verticalTabsStyleMobile :: Css
verticalTabsStyleMobile = ".tabs-vertical" ? tabsStyle'

verticalTabsStyle :: Css
verticalTabsStyle = do
  ".tabs-vertical-container" ? display flex

  ".tabs-vertical" ? do
    margin nil (rem (3 / 2)) nil nil
    paddingAll (rem (1 / 4))

    li ** a ? do
      display flex
      cursor pointer
      margin nil (rem (3 / 2)) nil nil
      padding (rem (1 / 2)) (rem (1 / 2)) (rem (1 / 2)) (rem (1 / 2))
      textDecoration none
      fontColor nord3'
      minWidth (rem 10)
      lineHeight (rem 1)
      borderLeft solid 3 white0'

      Clay.span ? textOverflowEllipsis

      hover Clay.& Clay.not (star # ".disabled") Clay.& do
        background nord6'
        borderLeft solid 3 nord10'

      ".error" Clay.& do
        important $ borderLeftColor nord11'

      ".success" Clay.& do
        important $ borderLeftColor green1'

      ".disabled" Clay.& do
        fontColor grey0'
        cursor notAllowed

    li ? do
      display block

      ".active" Clay.& a ? do
        important $ background nord4'
        fontColor nord0'
        borderLeft solid 3 nord10'

textOverflowEllipsis :: Css
textOverflowEllipsis = do
  overflow hidden
  whiteSpace nowrap
  textOverflow overflowEllipsis

tabsVertical
  :: (Ord k, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => Map k (Text, m ())
  -> m ()
tabsVertical =
  elClass "div" "tabs-vertical-container" . tabDisplay "tabs-vertical" "active"

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
      zIndex 10
      content (stringContent "")
      position fixed
      top nil
      right nil
      bottom nil
      left nil
      important $ backgroundColor nord0'
      important $ borderColor nord0'
      opacity 0.5
      borderWidth nil
      height (vh 100)
      width (vw 100)

  "#nav-secondary" # checked |+ nav ? right nil

  ".nav-opener" # checked |+ nav ? do
    display block
    position absolute
    top (rem 3)
    zIndex 20
    paddingTop (rem 1)
    minWidth (rem 15)
    height (vh 100 @-@ rem 4)

  nav ? do
    display none
    backgroundColor nord4'
    fontColor nord3'

    ".nav-group" ? do
      paddingLeft nil
      label ? do
        paddingLeft (rem 0.6)
        hover Clay.& backgroundColor grey0'

      ".angle-icon" ? do
        marginAll (rem 0.2)
        important $ width (rem 1)
        important $ height (rem 1)

    a ? do
      ".active" Clay.& backgroundColor white0'

      hover Clay.& backgroundColor grey0'

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

    ".icon" ? paddingRight (rem 0.6)

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

    li ? display block

    a ? do
      display block
      lineHeight (rem 2)

    label ? do
      display flex
      justifyContent spaceBetween
      cursor pointer
      lineHeight (rem 2)

    (Clay.span <> a) ? textOverflowEllipsis

  ".nav-group" ? do
    paddingRight nil
    ".angle-icon" ? do
      marginLeft (rem (-0.8))
      marginRight (rem 0.2)
      transforms [translate (rem 0.7) (rem 0.7), rotate (deg 90)]

    label ? paddingRight (rem 0.6)

    input # Clay.not ":checked" |+ star ? do
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
      hover Clay.& background nord6'

      ".active" Clay.& background nord4'

    ".nav-group" ? do
      a ? do
        fontWeight normal
        lineHeight (rem 1.5)

      label ? do
        justifyContent flexStart
        lineHeight (rem 1.5)
        marginLeft (rem (-0.8))
        ".angle-icon" ? order (-1)


sideNav :: DomBuilder t m => m a -> m a
sideNav = elClass "nav" "sidenav"

treeviewStyle :: Css
treeviewStyle = ".treeview" ? do
  fontSize (rem (13 / 16))
  ".icon" # Clay.not ".angle-icon" ? paddingRight (rem (1 / 4))

  ul ? do
    margin nil nil nil (rem (3 / 2))
    flexDirection column
    paddingAll nil

  li ? do
    display flex
    alignItems center
    lineHeight (rem 1.5)

  label ? do
    fontWeight (weight 500)
    display flex
    justifyContent flexStart
    cursor pointer
    lineHeight (rem 1.5)

    Clay.span ? do
      display flex
      alignItems center

  ".angle-icon" ? do
    order (-1)
    marginLeft (rem (-3 / 2))
    marginRight (rem (1 / 2))

  input # checked |+ star ? do
    ".angle-icon"
      ? transforms [translate (rem 0.25) (rem 0.25), rotate (deg 180)]
    ul ? display flex

  input # Clay.not (star # checked) |+ star ? do
    ".angle-icon" ? transforms [translate (rem 0.5) (rem 0.25), rotate (deg 90)]
    ul ? display none


treeview
  :: (MonadIO m, PostBuild t m, DomBuilder t m)
  => Event t Bool
  -> m ()
  -> m a
  -> m a
treeview = navGroup' False 1 "treeview"

leaf :: (DomBuilder t m) => m a -> m a
leaf = el "li"

