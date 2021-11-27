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
  , wizard
  , wizardPage
  , modalHeader
  , modalFooter
  , wizardFooter
  )
where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( icon )
import qualified Clay.Media                    as Media
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Default
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Reflex.Dom              hiding ( display
                                                , (&)
                                                )

import           Components.Accordion           ( StepperState(..) )
import           Components.Button
import           Components.Class
import           Components.Icon
import           Nordtheme

cardStyle :: Css
cardStyle =
  cardStyle'
    <> clickableCardStyle
    <> gridStyle
    <> modalStyle
    <> wizardStyle
    <> wizardStyleMobile

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

    Clay.button # firstChild ? do
      marginLeft (rem (3 / 4))

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
    zIndex 20
    left nil
    top nil
    width (pct 100)
    height (pct 100)
    overflow auto
    backgroundColor (setA 0.5 nord0')
    display flex
    justifyContent center
    flexDirection column

    ".card" ? do
      boxShadow (pure none)

    ".button-close" ? do
      marginRight nil
      marginTop nil
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

  ".modal-header" ? do
    star ? marginTop nil
    display flex
    justifyContent spaceBetween
    flexDirection row

  ".modal" |> star ? do
    paddingAll (rem (3 / 4))
    "margin" -: "1rem auto"
    maxWidth (pct 100 @-@ rem (7 / 2))
    maxHeight (pct 100 @-@ rem (7 / 2))

  ".modal" # ".small" |> star ? do
    width (rem 18)

  ".modal" # ".small" |> ".wizard" ? do
    height (rem 18)

  ".modal" # ".medium" |> star ? do
    width (rem 36)

  ".modal" # ".medium" |> ".wizard" ? do
    height (rem 18)

  ".modal" # ".large" |> star ? do
    width (rem 54)

  ".modal" # ".large" |> ".wizard" ? do
    height (rem 36)

  ".modal" # ".xtralarge" |> star ? do
    width (rem 72)

  ".modal" # ".xtralarge" |> ".wizard" ? do
    height (rem 54)


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


modalHeader :: (DomBuilder t m, PostBuild t m) => m () -> m (Event t ())
modalHeader titl = elClass "div" "modal-header" $ el "h3" titl >> modalCloseBtn

modalFooter
  :: (DomBuilder t m, PostBuild t m)
  => m (Event t b)
  -> m (Event t (), Event t b)
modalFooter okBtn = cardFooter $ do
  cancelEv <- cardAction "Cancel"
  okEv     <- okBtn
  pure (cancelEv, okEv)

wizardFooter
  :: (DomBuilder t m, PostBuild t m)
  => m (Event t b)
  -> m (Event t (), Event t (), Event t b)
wizardFooter okBtn = cardFooter $ do
  cancelEv <- cardAction "Cancel"
  backEv   <- btn
    def { _buttonConfig_priority = ButtonTertiary
        , _buttonConfig_class    = "wizard-back"
        }
    (text "Back")
  okEv <- okBtn
  pure (cancelEv, backEv, okEv)

type PageT t m k w a = ReaderT (Demux t k) (WriterT (Map k w) (StateT k m)) a

pageT
  :: (Ord k, Num k, DomBuilder t m, PostBuild t m)
  => Text
  -> m (a, w)
  -> PageT t m k w a
pageT cls cnt = ReaderT $ \d -> WriterT $ StateT $ \k -> do
  let isSelected = demuxed d k
      attrs      = ffor isSelected $ \s' -> "class" =: cls <> if s'
        then Map.empty
        else Map.singleton "style" "display:none;"
  (x, w) <- elDynAttr "div" attrs cnt
  pure ((x, k =: w), k + 1)

-- | Hide the wizard table of content on mobile layouts
wizardStyleMobile :: Css
wizardStyleMobile = do
  query Clay.all [Media.maxWidth 768] $ ".wizard-side" ? display none
  query Clay.all [Media.minWidth 768] $ ".wizard-back" ? display none

wizardStyle :: Css
wizardStyle = ".wizard" ? do
  display flex
  flexDirection row
  alignItems stretch
  "justify-items" -: "stretch"
  paddingAll nil

  ".wizard-side" ? do
    borderRight solid (px 1) grey0'
    backgroundColor (lighten 0.5 nord6')
    width (rem 12)
    paddingRight nil

    ".stepper" ? ".icon" ? paddingTop nil

  ".wizard-title" ? do
    marginTop nil
    fontSize (rem (3 / 2))
    marginBottom (rem 1)

  ".tabs-vertical" ? do
    paddingAll nil
    marginRight nil

  ".stepper" Clay.** a ? do
    borderLeft solid 3 (lighten 0.5 grey0')
    marginRight nil

  ".wizard-page" ? do
    borderTopWidth nil
    display flex
    flexDirection column
    flexGrow 1

  ".card-content" ? do
    flexGrow 1

type WizardPage t m a
  = PageT t m Integer (Text, Event t StepperState, Event t (), Event t ()) a

wizardPage
  :: (DomBuilder t m, PostBuild t m)
  => Text
  -> m (a, Event t StepperState, Event t (), Event t ())
  -> WizardPage t m a
wizardPage titl cnt = pageT "wizard-page" $ do
  xEv                        <- modalHeader (text titl)
  (x, nextEv, backEv, clsEv) <- cnt
  pure (x, (titl, nextEv, backEv, leftmost [clsEv, xEv]))

wizard
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => Text
  -> WizardPage t m (Dynamic t a)
  -> m (Event t (Maybe a))
wizard titl xs = elClass "div" "card wizard" $ do
  rec setPageEv1 <- elClass "div" "wizard-side" $ do
        elClass "h2" "wizard-title" $ text titl
        elClass "ul" "tabs-vertical stepper" $ mkLinks dynLinks demuxPage
      ((a, evs), _) <- runStateT (runWriterT (runReaderT xs demuxPage)) 0

      let (okEv, clsEv, setPageEv) =
            Map.foldlWithKey' foldEv (never, never, never) evs
      let dynLinks = constDyn evs
      dynPage <- holdDyn 0 (leftmost [setPageEv, setPageEv1])
      let demuxPage = demux dynPage
  pure (leftmost [Nothing <$ clsEv, Just <$> tagPromptlyDyn a okEv])

 where
  foldEv (_, clsEv0, setPage0) k (_, okEv, backEv, clsEv) =
    let filteredOkEv = ffilter (== StepperSuccess) okEv
    in
      ( () <$ filteredOkEv
      , leftmost [clsEv0, clsEv]
      , leftmost
        [setPage0, k + 1 <$ filteredOkEv, ffilter (>= 0) (k - 1 <$ backEv)]
      )

  mkLinks dynLinks demuxPage =
    fmap (fst . Map.findMin) <$> listViewWithKey dynLinks (mkLink demuxPage)

  mkLink demuxPage k dynVal = do
    stateDyn <- holdDyn StepperNeutral
                        (switchDyn $ (\(_, x, _, _) -> x) <$> dynVal)

    let dynActive = demuxed demuxPage k
    curActive        <- sample (current dynActive)
    historyActiveDyn <- foldDyn (||) curActive (updated dynActive)

    let stateClsDyn =
          stateCls <$> stateDyn <*> (Prelude.not <$> historyActiveDyn)
    let activeClsDyn = activeCls <$> dynActive

    clickEv <- elDynClass "li" activeClsDyn $ do
      (l, _) <- elDynClass' "a" stateClsDyn $ do
        _ <- elClass "span" "stepnum" $ dyn (numOrIcon k <$> stateDyn)
        el "span" $ dynText ((\(x, _, _, _) -> x) <$> dynVal)
      pure $ domEvent Click l

    pure (gate (current historyActiveDyn) $ k <$ clickEv)

  stateCls _              True = "disabled"
  stateCls StepperNeutral _    = ""
  stateCls StepperError   _    = "error"
  stateCls StepperSuccess _    = "success"

  activeCls True  = "active"
  activeCls False = ""

  numOrIcon _ StepperError =
    icon def { _iconConfig_status = constDyn (Just Danger) } errorStandardIcon
  numOrIcon k _ = text ((<> ".") . pack . show $ k)
