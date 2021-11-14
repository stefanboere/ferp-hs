{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Components.Input.Basic
  ( hiddenInput
  , noInput
  , numberInput
  , integralInput
  , toggleInput
  , togglesInput
  , checkboxInput
  , checkboxesInput
  , checkboxesInputMap
  , checkboxesInputDynMap
  , checkboxInputSimple
  , inputStyle
  , InputConfig'(..)
  , InputConfig
  , NumberInputConfig
  , EnumInputConfig
  , OpElem(..)
  , Elem
  , DomInputEl
  , inputConfig
  , inputConfig'
  , textInput
  , textInputEl
  , textInputWithIco
  , textInputWithIco'
  , textInputWithIco''
  , InputStatus(..)
  , InputEl(..)
  , labeled
  , labeledDyn
  , randomId
  , holdDynAttributes
  , NumberRange(..)
  , selectInput
  , allPossibleMap
  , textAreaInput
  , rangeInput
  , radioInput
  , fileInput
  , datalistInput
  , selectIcon
  , inputGroup
  , passwordInput
  , timeInput
  , timeOfDayInput
  , dateInput
  , localtimeInput
  , weekInput
  , monthInput
  , statusModAttrEv'
  , statusMessageDiv
  , requiredInput
  , statusMessageIcon
  , statusMessageElement
  , colorCls
  )
where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( (&)
                                                , empty
                                                , icon
                                                , max
                                                , not
                                                , selectElement
                                                )
import qualified Clay                           ( (&) )
import qualified Clay.Media                    as Media
import           Clay.Stylesheet                ( key )
import           Control.Applicative            ( (<|>)
                                                , Const(..)
                                                )
import           Control.Lens                   ( (%~) )
import           Control.Monad                  ( join )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Default
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Data.Monoid                    ( Any(..) )
import           Data.Proxy
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.String                    ( IsString )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Time
import           Data.Time.Calendar.WeekDate
import qualified GHCJS.DOM.Types               as DOM
                                                ( File )
import           Reflex
import           Reflex.Dom              hiding ( fileInput
                                                , rangeInput
                                                , textInput
                                                )
import           System.Random                  ( StdGen
                                                , getStdGen
                                                , random
                                                , setStdGen
                                                )
import           Text.Printf                    ( printf )
import           Text.Read                      ( readMaybe )

import           Components.Class
import           Components.Icon
import           Nordtheme

data InputStatus = InputNeutral (Maybe Text) | InputSuccess Text | InputError Text | InputDisabled deriving (Eq, Ord)

instance Default InputStatus where
  def = InputNeutral Nothing

instance Semigroup InputStatus where
  (InputNeutral x) <> (InputNeutral y) = InputNeutral (x <> y)
  (InputNeutral _) <> x                = x
  x                <> _                = x

type InputConfig = InputConfig' (Const ())

type NumberInputConfig t = InputConfig' (NumberRange t) t

newtype OpElem m a = OpElem { getOpElem :: Elem a -> m () }

-- | Strips away the functor type
type family Elem a where
  Elem (f x) = x
  Elem x = x

-- | To show the enum values, we need a list of labels for each enum value.
-- That is, the extra config is a map 'a -> m ()'. This is precisely 'Op (m ()) a'
type EnumInputConfig t m = InputConfig' (OpElem m) t m

type DomInputEl t m = InputEl (DomBuilderSpace m) t

data InputConfig' extraConfig t m a = InputConfig
  { _inputConfig_initialValue     :: a
  , _inputConfig_setValue         :: Event t a
  , _inputConfig_id               :: Maybe Text
  , _inputConfig_status           :: Dynamic t InputStatus
  , _inputConfig_attributes       :: Map AttributeName Text
  , _inputConfig_modifyAttributes :: Event t (Map AttributeName (Maybe Text))
  , _inputConfig_eventSpec
      :: EventSpec (DomBuilderSpace m) EventResult
      -> EventSpec (DomBuilderSpace m) EventResult
  , _inputConfig_extra :: extraConfig a
  }

-- | The result of an input widget
data InputEl d t a = InputEl
  { _inputEl_value    :: Dynamic t a
  , _inputEl_hasFocus :: Dynamic t Bool
  , _inputEl_elements :: Dynamic t [Element EventResult d t] -- ^ An input widget could consist of multiple input elements; eg. a combined range and number input
  }

instance (Reflex t, Semigroup a) => Semigroup (InputEl d t a) where
  x <> y = addInputElWith (<>) x y

addInputElWith
  :: Reflex t
  => (Dynamic t a -> Dynamic t b -> Dynamic t c)
  -> InputEl d t a
  -> InputEl d t b
  -> InputEl d t c
addInputElWith addFn x y = InputEl
  { _inputEl_value    = _inputEl_value x `addFn` _inputEl_value y
  , _inputEl_hasFocus = getAny
                        <$> (Any <$> _inputEl_hasFocus x)
                        <>  (Any <$> _inputEl_hasFocus y)
  , _inputEl_elements = _inputEl_elements x <> _inputEl_elements y
  }

instance (Reflex t, Monoid a) => Monoid (InputEl d t a) where
  mempty = hiddenInput (constDyn mempty)
  mconcat xs = InputEl
    { _inputEl_value    = mconcat (_inputEl_value <$> xs)
    , _inputEl_hasFocus = getAny
                            <$> mconcat (fmap (fmap Any . _inputEl_hasFocus) xs)
    , _inputEl_elements = mconcat (fmap _inputEl_elements xs)
    }

instance Reflex t => Functor (InputEl d t) where
  fmap f x = x { _inputEl_value = f <$> _inputEl_value x }

instance Reflex t => Applicative (InputEl d t) where
  pure x = hiddenInput (constDyn x)
  x <*> y = addInputElWith (<*>) x y

-- | An input which is never focussed and consists of zero elements
--
--      hiddenInput (constDyn x) == pure x
--      hiddenInput (constDyn mempty) == mempty
hiddenInput :: Reflex t => Dynamic t a -> InputEl d t a
hiddenInput x = InputEl { _inputEl_value    = x
                        , _inputEl_hasFocus = constDyn False
                        , _inputEl_elements = constDyn []
                        }

-- | Same as @hiddenInput@, but with an InputConfig object
noInput
  :: (Reflex t, MonadHold t m) => InputConfig' c t m a -> m (DomInputEl t m a)
noInput cfg = hiddenInput
  <$> holdDyn (_inputConfig_initialValue cfg) (_inputConfig_setValue cfg)

inputConfig'
  :: (Reflex t, DomSpace (DomBuilderSpace m))
  => c a
  -> a
  -> InputConfig' c t m a
inputConfig' extra initval = InputConfig
  { _inputConfig_initialValue     = initval
  , _inputConfig_setValue         = never
  , _inputConfig_id               = def
  , _inputConfig_status           = def
  , _inputConfig_attributes       = def
  , _inputConfig_modifyAttributes = never
  , _inputConfig_extra            = extra
  , _inputConfig_eventSpec        = def
  }

inputConfig
  :: (Default (c a), Reflex t, DomSpace (DomBuilderSpace m))
  => a
  -> InputConfig' c t m a
inputConfig = inputConfig' def

instance Default (Const () a) where
  def = Const ()

instance (Default a, Default (c a), Reflex t, DomSpace (DomBuilderSpace m)) => Default (InputConfig' c t m a) where
  def = inputConfig' def def

instance (Functor c, Reflex t) => Functor (InputConfig' c t m) where
  fmap f cfg = cfg
    { _inputConfig_initialValue = f $ _inputConfig_initialValue cfg
    , _inputConfig_setValue     = f <$> _inputConfig_setValue cfg
    , _inputConfig_extra        = f <$> _inputConfig_extra cfg
    }

data NumberRange t a = NumberRange
  { _numberRange_maxValue  :: Dynamic t (Maybe a)
  , _numberRange_minValue  :: Dynamic t (Maybe a)
  , _numberRange_precision :: Maybe Int
  }

instance Reflex t => Default (NumberRange t a) where
  def = NumberRange def def Nothing

instance Reflex t => Functor (NumberRange t) where
  fmap f cfg = cfg
    { _numberRange_maxValue = fmap f <$> _numberRange_maxValue cfg
    , _numberRange_minValue = fmap f <$> _numberRange_minValue cfg
    }

inputStyle :: Css
inputStyle = do
  formStyle
  inputElementStyle
  selectElementStyle
  textAreaElementStyle
  rangeElementStyle
  checkboxStyle
  toggleStyle
  radioStyle
  fileUploadStyle
  datalistStyle
  inputGroupStyle
  timeStyle
  ".absolute" ? position absolute

  ".flex-row" ? do
    Clay.display flex
    flexDirection row
    position relative
    flexGrow 1

  ".input" ? do
    Clay.display flex
    flexDirection column
    verticalAlign vAlignTop
    flexGrow 1

  ".nopointer" ? do
    pointerEvents none
    cursor cursorDefault

  input # ("type" @= "submit") ? do
    important $ background nord7'
    justifyContent center
    important $ marginRight nil
    hover Clay.& enabled Clay.& do
      important $ background (rgb 121 184 202)

datalistStyle :: Css
datalistStyle = do
  datalist ? Clay.display none
  ".datalist" # "::-webkit-calendar-picker-indicator" ? Clay.display none

timeStyle :: Css
timeStyle =
  (  input
    #  ("type" @= "time")
    <> input
    #  ("type" @= "password")
    <> input
    #  ("type" @= "date")
    <> input
    #  ("type" @= "datetime-local")
    <> input
    #  ("type" @= "month")
    <> input
    #  ("type" @= "week")
    )
    ? paddingRight (rem (3 / 2))

toggleStyle :: Css
toggleStyle = input # ("type" @= "checkbox") # ".toggle" ? do
  marginLeft (rem (1 / 2))
  marginRight (rem 1.5)

  before Clay.& do
    left (rem (-0.3))
    width (rem 2.2)
    height (rem 1.1)
    backgroundColor nord3'
    borderWidth nil
    borderRadiusAll (rem 1)
    transitionDuration 0.1
    transitionTimingFunction easeIn

    disabled Clay.& backgroundColor grey0'

    checked Clay.& do
      backgroundColor nord10'
      transitionDuration 0.1
      transitionTimingFunction easeIn

      disabled Clay.& backgroundColor (lighten 0.5 nord10')

  after Clay.& do
    absoluteBlock
    width (rem 0.825)
    height (rem 0.825)
    left (rem (-0.20))
    top (rem 0.06)
    borderRadiusAll (pct 50)
    background white0'
    transitionDuration 0.1
    transitionTimingFunction easeIn

    checked Clay.& do
      left (rem 0.925)
      borderWidth nil
      transitionDuration 0.1
      transitionTimingFunction easeIn

checkboxStyle :: Css
checkboxStyle = do
  ".checkbox-label" ? do
    cursor pointer
    fontWeight normal

  (input # ("type" @= "checkbox") <> input # ("type" @= "radio"))
    #  disabled
    |+ label
    ?  do
         fontColor grey0'
         cursor notAllowed

  (  input
    #  ("type" @= "checkbox")
    <> input
    #  ("type" @= "radio")
    <> ".selected-count"
    )
    ? do
        "-moz-appearance" -: "initial"  -- Hack to use checkbox pseudo in firefox
        position relative
        cursor pointer
        marginRight (rem 0.8)
        width (rem (13 / 16))
        height (rem (13 / 16))
        paddingAll nil

        disabled Clay.& cursor notAllowed

        before Clay.& do
          absoluteBlock
          width (rem 1)
          height (rem 1)
          borderRadiusAll (px 3)
          border solid 1 grey0'
          backgroundColor white0'
          top (rem (-3 / 32))
          left nil
          ".has-error" Clay.& borderColor nord11'
          focus Clay.& do
            borderColor nord10'
            boxShadow . pure $ bsColor nord10' $ shadowWithBlur nil
                                                                nil
                                                                (rem (1 / 4))

        checked Clay.& do
          before Clay.& do
            borderColor nord10'
            backgroundColor nord10'
            disabled Clay.& do
              borderColor grey0'
              backgroundColor grey0'

  (  input
    #  ("type" @= "checkbox")
    #  checked
    <> input
    #  ("type" @= "radio")
    #  checked
    <> ".selected-count"
    )
    ? do
        after Clay.& do
          absoluteBlock
          width (rem 0.25)
          height (rem 0.5)
          borderStyle solid
          borderColor white0'
          borderWidth4 nil 2 2 nil
          transform (rotate (deg 45))
          top (rem 0.1)
          left (rem 0.4)

radioStyle :: Css
radioStyle = input # ("type" @= "radio") ? do

  before Clay.& do
    borderRadiusAll (pct 50)
    left (rem (-0.1))

  checked Clay.& after Clay.& do
    width (rem 0.25)
    height (rem 0.25)
    left (rem 0.25)
    top (rem 0.3)
    background white0'
    borderRadiusAll (pct 50)

    checked Clay.& absoluteBlock


inputElementStyle :: Css
inputElementStyle =
  (input <> Clay.select <> textarea <> ".dropzone" <> ".select") ? do
    background transparent
    borderWidth 0
    borderBottomWidth 1
    borderBottomStyle solid
    paddingAll (rem 0.25)
    borderColor grey0'
    outlineWidth 0
    outlineStyle none
    flexGrow 1
    fontColor inherit
    "font" -: "inherit"
    boxSizing contentBox
    borderRadiusAll nil
    marginAll nil

    focus Clay.& do
      borderBottomWidth 2
      borderColor nord10'
      marginBottom (px (-1))

    "::placeholder" Clay.& fontColor grey0'

    disabled Clay.& do
      fontColor grey0'
      cursor notAllowed

    ".has-error" Clay.& do
      important $ borderColor nord11'

    ".has-success" Clay.& do
      important $ borderColor green1'

selectElementStyle :: Css
selectElementStyle = do
  (Clay.select <> ".select") ? do
    paddingRight (rem (3 / 2))
    cursor pointer
    flexGrow 1
    "appearance" -: "none"
    "-webkit-appearance" -: "none"
    "-moz-appearance" -: "none"

  (input <> Clay.select <> ".select") |+ ".input-icon" ? do
    Clay.display inlineBlock
    left (rem (-1.2))
    top (rem 0.2)
    position relative
    ".icon" ? position absolute
    cursor pointer

textAreaElementStyle :: Css
textAreaElementStyle = (".dropzone" <> textarea) ? do
  background white
  borderWidth 1
  borderRadiusAll (px 3)
  minHeight (rem 6)

  disabled Clay.& do
    background white0'
    cursor notAllowed

rangeElementStyle :: Css
rangeElementStyle = do

  input # ("type" @= "range") ? do
    "-webkit-appearance" -: "none"
    paddingAll nil
    height (Clay.rem 0.2)
    borderBottomWidth nil
    cursor pointer
    background nord10'
    disabledStyle
    marginTop (rem (2 / 3))
    borderColor nord10'

    "::-webkit-slider-thumb" Clay.& do
      "-webkit-appearance" -: "none"
      thumb

    "::-moz-range-thumb" Clay.& do
      borderWidth nil
      backgroundColor inherit

 where
  disabledStyle :: Css
  disabledStyle = disabled Clay.& do
    background grey0'
    cursor notAllowed

  thumb = do
    height (rem 1)
    width (rem 1)
    borderRadiusAll (pct 50)
    background nord10'
    cursor pointer

    disabledStyle

fileUploadStyle :: Css
fileUploadStyle = label # ".file-upload-label" ? do
  marginTop nil
  marginBottom nil
  marginRight (rem (1 / 4))
  input # ("type" @= "file") ? Clay.display none

  ".disabled" Clay.& do
    cursor notAllowed
    borderColor grey0'
    fontColor grey0'
    "fill" -: showColor grey0'

inputGroupStyle :: Css
inputGroupStyle = ".input-group" ? do
  label ? Clay.display none

  ".has-error" Clay.& do
    (Clay.select <> textarea <> input <> ".drozone" <> ".select")
      ? borderColor nord11'

  ".has-success" Clay.& do
    (Clay.select <> textarea <> input <> ".dropzone" <> ".select")
      ? borderColor green1'

formStyle :: Css
formStyle = do
  query Clay.all [Media.maxWidth 768] (mobileFormStyle form)

  label ? do
    fontWeight bold
    lineHeight (rem 1.5)

  form |> label ? do
    "grid-column" -: "1 / 2"

  form |> (input <> ".input") ? do
    "grid-column" -: "2 / 3"

  form ? do
    Clay.display grid
    maxWidth (rem 30)
    key "grid-column-gap" (rem 2)
    key "grid-row-gap"    (rem 0.25)
    "grid-template-columns" -: "10rem 1fr"

  ".helptext" ? do
    fontSize (rem 0.75)
    marginTop (rem (1 / 4))

  ".helptext" # ".has-error" ? fontColor nord11'

  ".helptext" # ".has-success" ? fontColor green1'

  ".statusmessage" ? do
    Clay.display flex
    alignItems center
    ".icon" ? position relative

  mobileFormStyle (form # ".vertical")

mobileFormStyle :: Selector -> Css
mobileFormStyle formSel = do
  formSel ? do
    "grid-template-columns" -: "1fr"
    key "grid-row-gap" (rem 0)

  formSel |> label ? do
    marginTop (rem 1)
    lineHeight (rem 1)
    "grid-column" -: "1 / -1"

  formSel |> (input <> ".input") ? do
    "grid-column" -: "1 / -1"


-- | Class to be added when the input is in a certain state
colorCls :: InputStatus -> Text
colorCls (InputSuccess _) = "has-success"
colorCls (InputError   _) = "has-error"
colorCls _                = ""

-- | Generate a random id string
randomId :: MonadIO m => m Text
randomId = do
  stdGen <- liftIO getStdGen
  let (idInt, stdGen') = random stdGen :: (Int, StdGen)
  liftIO $ setStdGen stdGen'
  pure $ pack (printf "%x" idInt)

statusModAttrEv'
  :: (PostBuild t m)
  => InputConfig' c t m a
  -> m (Event t (Map AttributeName (Maybe Text)))
statusModAttrEv' cfg = statusModAttrEv
  (_inputConfig_attributes cfg Map.!? "class")
  (_inputConfig_status cfg)

statusModAttrEv
  :: (PostBuild t m)
  => Maybe Text
  -> Dynamic t InputStatus
  -> m (Event t (Map AttributeName (Maybe Text)))
statusModAttrEv mCls status = do
  postBuildEv <- getPostBuild
  let postBuildAttrEv =
        attachPromptlyDynWith (\x _ -> statusAttrs x) status postBuildEv

  pure $ mergeWith (<>) [modAttrEv, postBuildAttrEv]

 where
  modAttrEv = updated (statusAttrs <$> status)

  classStr (Just x) y = x <> " " <> y
  classStr _        y = y

  statusAttrs :: InputStatus -> Map AttributeName (Maybe Text)
  statusAttrs state =
    "class"
      =: Just (classStr mCls (colorCls state))
      <> "disabled"
      =: if state == InputDisabled then Just "" else Nothing

statusMessageIcon
  :: (PostBuild t m, DomBuilder t m) => Dynamic t InputStatus -> m ()
statusMessageIcon = dyn_ . fmap mkIcon
 where
  cfg = def { _iconConfig_size = 1.5 }
  mkIcon (InputError _) = icon
    cfg { _iconConfig_status = constDyn $ Just Danger }
    exclamationCircleIcon
  mkIcon (InputSuccess _) =
    icon cfg { _iconConfig_status = constDyn $ Just Success } checkCircleIcon
  mkIcon _ = blank

statusMessageElement
  :: (PostBuild t m, DomBuilder t m) => Dynamic t InputStatus -> m ()
statusMessageElement status = do
  let dynClass =
        (\state -> "class" =: (colorCls state <> " helptext")) <$> status
  let dynMessage = message <$> status
  elDynAttr "div" dynClass $ dynText dynMessage

 where
  message (InputError   x       ) = x
  message (InputSuccess x       ) = x
  message (InputNeutral (Just x)) = x
  message _                       = ""

-- | Creates a label with a random for string, which is returned
labelFor
  :: (PostBuild t m, DomBuilder t m, MonadIO m) => Dynamic t Text -> m Text
labelFor dynLabel = do
  idStr <- randomId
  elAttr "label" ("for" =: idStr) $ dynText dynLabel
  pure idStr

-- | An editor with a label. Creates a random id and adds it to the editor
labeled
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => Text
  -> (InputConfig' c t m a -> m b)
  -> InputConfig' c t m a
  -> m b
labeled lbl = labeledDyn (constDyn lbl)

labeledDyn
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => Dynamic t Text
  -> (InputConfig' c t m a -> m b)
  -> InputConfig' c t m a
  -> m b
labeledDyn lbl editor cfg = do
  idStr <- labelFor lbl
  editor cfg { _inputConfig_id = Just idStr }

textInput
  :: forall t m
   . (PostBuild t m, DomBuilder t m)
  => InputConfig t m Text
  -> m (DomInputEl t m Text)
textInput cfg = elClass "div" "input" $ do
  n <- elClass "div" "flex-row" $ do
    n' <- textInputEl cfg

    statusMessageIcon (_inputConfig_status cfg)
    pure n'
  statusMessageElement (_inputConfig_status cfg)

  pure n

textInputEl
  :: forall t m
   . (PostBuild t m, DomBuilder t m)
  => InputConfig t m Text
  -> m (DomInputEl t m Text)
textInputEl cfg = do
  modAttrEv <- statusModAttrEv' cfg
  n         <-
    inputElement
    $  (def :: InputElementConfig EventResult t (DomBuilderSpace m))
    &  inputElementConfig_initialValue
    .~ _inputConfig_initialValue cfg
    &  inputElementConfig_setValue
    .~ _inputConfig_setValue cfg
    &  inputElementConfig_elementConfig
    .  elementConfig_initialAttributes
    .~ _inputConfig_attributes cfg
    <> idAttr (_inputConfig_id cfg)
    &  inputElementConfig_elementConfig
    .  elementConfig_modifyAttributes
    .~ mergeWith (<>) [modAttrEv, _inputConfig_modifyAttributes cfg]
    &  inputElementConfig_elementConfig
    .  elementConfig_eventSpec
    %~ _inputConfig_eventSpec cfg
  pure $ InputEl { _inputEl_value    = _inputElement_value n
                 , _inputEl_hasFocus = _inputElement_hasFocus n
                 , _inputEl_elements = constDyn [_inputElement_element n]
                 }


textInputWithIco
  :: (PostBuild t m, DomBuilder t m, MonadFix m)
  => m (Event t (Map AttributeName (Maybe Text)), Event t Text)
  -> InputConfig t m Text
  -> m (DomInputEl t m Text)
textInputWithIco after' cfg = fst <$> textInputWithIco' (((), ) <$> after') cfg

textInputWithIco'
  :: forall t m a
   . (PostBuild t m, DomBuilder t m, MonadFix m)
  => m (a, (Event t (Map AttributeName (Maybe Text)), Event t Text))
  -> InputConfig t m Text
  -> m (DomInputEl t m Text, a)
textInputWithIco' after' cfg =
  (\(e, _, a') -> (e, a')) <$> textInputWithIco'' "" (pure ()) after' cfg

textInputWithIco''
  :: forall t m a b
   . (PostBuild t m, DomBuilder t m, MonadFix m)
  => Text
  -> m b
  -> m (a, (Event t (Map AttributeName (Maybe Text)), Event t Text))
  -> InputConfig t m Text
  -> m (DomInputEl t m Text, b, a)
textInputWithIco'' cls before' after' cfg = do

  let dynStatusCls = mkCls <$> _inputConfig_status cfg

  elClass "div" "input" $ do
    (n, bf, af) <- elClass "div" "flex-row" $ do
      (n', bf', af') <-
        elDynClass "div" (joinCls ["flex-row", cls] <$> dynStatusCls) $ do
          bf'' <- before'
          rec
            n'' <- textInputEl cfg
              { _inputConfig_setValue         = leftmost
                                                  [setValEv, _inputConfig_setValue cfg]
              , _inputConfig_modifyAttributes = mergeWith
                (<>)
                [_inputConfig_modifyAttributes cfg, attrEv]
              }

            (af'', (attrEv, setValEv)) <- after'
          pure (n'', bf'', af'')

      statusMessageIcon (_inputConfig_status cfg)
      pure (n', bf', af')

    statusMessageElement (_inputConfig_status cfg)

    pure (n, bf, af)

 where
  joinCls xs x = Text.unwords $ Prelude.filter (not . Text.null) (x : xs)
  mkCls InputDisabled = "disabled"
  mkCls x             = colorCls x


numberInput
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , RealFloat a
     )
  => NumberInputConfig t m a
  -> m (DomInputEl t m (Maybe a))
numberInput = numberRangeInput True

integralInput
  :: forall t m a
   . (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m, Integral a)
  => NumberInputConfig t m a
  -> m (DomInputEl t m (Maybe a))
integralInput cfg = conv <$> numberInput
  (fromIntegral <$> cfg
    { _inputConfig_extra = (_inputConfig_extra cfg) { _numberRange_precision = Just
                                                      0
                                                    }
    }
  )
 where
  conv :: DomInputEl t m (Maybe Double) -> DomInputEl t m (Maybe a)
  conv = fmap (fmap Prelude.round)

numberRangeInput
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , RealFloat a
     )
  => Bool
  -> NumberInputConfig t m a
  -> m (DomInputEl t m (Maybe a))
numberRangeInput isReg cfg = do
  (minMaxEv, minMaxDyn) <- getMinMaxEv prnt nc
  rec
    n <- textInput cfg
      { _inputConfig_initialValue     = prnt $ _inputConfig_initialValue cfg
      , _inputConfig_setValue         = leftmost
        [prnt <$> _inputConfig_setValue cfg, zeroIfEmptyEv]
      , _inputConfig_status = zipDynWith max (_inputConfig_status cfg) statusDyn
      , _inputConfig_attributes       = _inputConfig_attributes cfg <> initAttrs
      , _inputConfig_modifyAttributes = mergeWith
                                          (<>)
                                          [ _inputConfig_modifyAttributes cfg
                                          , minMaxEv
                                          , selectAttrEv
                                          ]
      , _inputConfig_extra            = Const ()
      }
    let
      result0       = fmap (readMaybe . unpack) n
      result        = _inputEl_value result0
      selectAttrEv  = if isReg then mkOnClick <$> updated result else never
      lostFocusEv   = ffilter Prelude.not (updated (_inputEl_hasFocus n))
      zeroIfEmptyEv = attachWithMaybe emptyNoFocus (current result) lostFocusEv
      modAttrEv     = updated
        (   styleChange
        <$> minMaxDyn
        <*> result
        <*> _inputEl_hasFocus n
        <*> _inputEl_value n
        )
    statusDyn <- holdDyn def modAttrEv

  return result0
 where
  initAttrs = Map.fromList $ mapMaybe
    (\(x, y) -> (x, ) <$> y)
    [ ("type" , if isReg then Just "number" else Just "range")
    , ("style", Just "text-align:right")
    , ( "onClick"
      , if _inputConfig_initialValue cfg == 0 && isReg
        then Just "this.select()"
        else Nothing
      )
    , ("step", mkStep <$> _numberRange_precision nc)
    ]

  styleChange (b_min, b_max) (Just x) _ _
    | maybe False (x >) b_max
    = InputError $ "Exceeds the maximum " <> prnt (fromJust b_max)
    | maybe False (x <) b_min
    = InputError $ "Is less than the minimum " <> prnt (fromJust b_min)
    | otherwise
    = def
  styleChange _ Nothing hasFocus t
    | Text.null t || hasFocus = def
    | -- Don't update the value when still typing
      otherwise               = InputError "Not a valid number"

  nc = _inputConfig_extra cfg

  mkOnClick x = "onClick"
    =: if isNothing x || x == Just 0 then Just "this.select()" else Nothing

  mkStep :: Int -> Text
  mkStep x = prnt (10 ^^ (-x))
  prnt = showFloat (_numberRange_precision nc)
  emptyNoFocus Nothing _ = Just (prnt 0)
  emptyNoFocus _       _ = Nothing

showFloat :: RealFloat a => Maybe Int -> a -> Text
showFloat Nothing x = pack (show (fromRational $ toRational x :: Double))
showFloat (Just precision) x
  | precision > 0
  = let magn  = 10 ^ precision
        r     = Prelude.round (abs x * 10 ^^ precision) :: Int
        r0    = r `Prelude.div` magn
        r1    = r `mod` magn
        r0Txt = pack (show r0)
        r1Txt = Text.justifyRight precision '0' $ pack (show r1)
        sgn   = if x >= 0 then "" else "-"
    in  sgn <> r0Txt <> "." <> r1Txt
  | otherwise
  = pack (show (Prelude.round x :: Integer))

toggleInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => Text
  -> InputConfig t m Bool
  -> m (DomInputEl t m Bool)
toggleInput lbl cfg = do
  let initAttrs = "class" =: "toggle"

  checkboxInput
    lbl
    cfg { _inputConfig_attributes = _inputConfig_attributes cfg <> initAttrs }

togglesInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m, Ord a, Enum a, Bounded a)
  => EnumInputConfig t m (Set a)
  -> m (DomInputEl t m (Set a))
togglesInput cfg = do
  let initAttrs = "class" =: "toggle"

  checkboxesInput cfg
    { _inputConfig_attributes = _inputConfig_attributes cfg <> initAttrs
    }

checkboxesInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m, Ord a, Enum a, Bounded a)
  => EnumInputConfig t m (Set a)
  -> m (DomInputEl t m (Set a))
checkboxesInput cfg = checkboxesInputMap
  (allPossibleMap toLabel)
  cfg { _inputConfig_extra = Const () }
  where toLabel = getOpElem (_inputConfig_extra cfg)

allPossibleMap :: (Bounded a, Enum a, Ord a) => (a -> b) -> Map a b
allPossibleMap toLabel =
  Map.fromList . fmap (\x -> (x, toLabel x)) $ allPossible (Proxy :: Proxy a)

idAttr :: (Ord a, IsString a) => Maybe Text -> Map a Text
idAttr = maybe mempty (Map.singleton "id")

-- | Same as 'checkboxesInput' but with a map containing all options
checkboxesInputMap
  :: (PostBuild t m, DomBuilder t m, MonadIO m, Ord a)
  => Map a (m ())
  -> InputConfig t m (Set a)
  -> m (DomInputEl t m (Set a))
checkboxesInputMap opts cfg =
  elAttr "div" ("class" =: "input" <> idAttr (_inputConfig_id cfg)) $ do
    modAttrEv <- statusModAttrEv' cfg

    result    <- Map.traverseWithKey (mkCheckbox modAttrEv) opts

    statusMessageDiv (_inputConfig_status cfg)

    let dynSet = Map.keysSet . Map.filter Prelude.id <$> joinDynThroughMap
          (constDyn $ fmap _inputEl_value result)

    pure $ InputEl
      { _inputEl_value    = dynSet
      , _inputEl_hasFocus = getAny
        <$> mconcat (fmap (fmap Any . _inputEl_hasFocus) . Map.elems $ result)
      , _inputEl_elements = mconcat . fmap _inputEl_elements $ Map.elems result
      }
 where
  mkCheckbox modAttrEv k x =
    let cfg' = ((k `elem`) <$> cfg)
    in  checkboxInput'
          x
          cfg'
            { _inputConfig_modifyAttributes = mergeWith
                                                (<>)
                                                [ modAttrEv
                                                , _inputConfig_modifyAttributes
                                                  cfg'
                                                ]
            }

-- | Same as 'checkboxesInput' but with a map containing all options
checkboxesInputDynMap
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadIO m
     , MonadHold t m
     , MonadFix m
     , Ord a
     )
  => Dynamic t (Map a (m ()))
  -> InputConfig t m (Set a)
  -> m (DomInputEl t m (Set a))
checkboxesInputDynMap opts cfg =
  elAttr "div" ("class" =: "input" <> idAttr (_inputConfig_id cfg)) $ do
    modAttrEv <- statusModAttrEv' cfg

    result    <- listWithKey opts (mkCheckbox modAttrEv)

    statusMessageDiv (_inputConfig_status cfg)

    let dynSet = Map.keysSet . Map.filter Prelude.id <$> joinDynThroughMap
          (fmap (fmap _inputEl_value) result)
    let dynFocus =
          joinDynThroughMap (fmap (fmap (fmap Any . _inputEl_hasFocus)) result)
    let dynElems = joinDynThroughMap (fmap (fmap _inputEl_elements) result)
    pure $ InputEl
      { _inputEl_value    = dynSet
      , _inputEl_hasFocus = getAny . mconcat . Map.elems <$> dynFocus
      , _inputEl_elements = mconcat . Map.elems <$> dynElems
      }
 where
  mkCheckbox modAttrEv k x =
    let cfg' = ((k `elem`) <$> cfg)
    in  checkboxInput'
          (dyn_ x)
          cfg'
            { _inputConfig_modifyAttributes = mergeWith
                                                (<>)
                                                [ modAttrEv
                                                , _inputConfig_modifyAttributes
                                                  cfg'
                                                ]
            }

statusMessageDiv
  :: (PostBuild t m, DomBuilder t m) => Dynamic t InputStatus -> m ()
statusMessageDiv status = elClass "div" "statusmessage" $ do
  statusMessageIcon status
  statusMessageElement status

checkboxInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => Text
  -> InputConfig t m Bool
  -> m (DomInputEl t m Bool)
checkboxInput lbl cfg = fmap (Prelude.not . Set.null) <$> checkboxesInputMap
  (Map.singleton () (text lbl))
  ((\x -> if x then Set.singleton () else Set.empty) <$> cfg)

checkboxInput'
  :: forall t m
   . (PostBuild t m, DomBuilder t m, MonadIO m)
  => m ()
  -> InputConfig t m Bool
  -> m (DomInputEl t m Bool)
checkboxInput' lbl cfg = el "div" $ do
  idStr <- randomId
  n     <-
    inputElement
    $  (def :: InputElementConfig EventResult t (DomBuilderSpace m))
    &  inputElementConfig_initialChecked
    .~ _inputConfig_initialValue cfg
    &  inputElementConfig_setChecked
    .~ _inputConfig_setValue cfg
    &  inputElementConfig_elementConfig
    .  elementConfig_initialAttributes
    .~ _inputConfig_attributes cfg
    <> ("type" =: "checkbox")
    <> "id"
    =: idStr
    &  inputElementConfig_elementConfig
    .  elementConfig_modifyAttributes
    .~ _inputConfig_modifyAttributes cfg
    &  inputElementConfig_elementConfig
    .  elementConfig_eventSpec
    %~ _inputConfig_eventSpec cfg

  elAttr "label" ("for" =: idStr <> "class" =: "checkbox-label") lbl

  pure $ InputEl { _inputEl_value    = _inputElement_checked n
                 , _inputEl_hasFocus = _inputElement_hasFocus n
                 , _inputEl_elements = constDyn [_inputElement_element n]
                 }

checkboxInputSimple
  :: DomBuilder t m
  => Bool
  -> Event t Bool
  -> Map AttributeName Text
  -> m (Dynamic t Bool)
checkboxInputSimple initOpen setOpen attrs = do
  r <-
    inputElement
    $            def
    Reflex.Dom.& inputElementConfig_setChecked
    .~           setOpen
    Reflex.Dom.& inputElementConfig_elementConfig
    .            elementConfig_initialAttributes
    .~           attrs
    <>           ("type" =: "checkbox")
    Reflex.Dom.& inputElementConfig_initialChecked
    .~           initOpen
  pure (_inputElement_checked r)

selectInput
  :: forall t m a
   . (PostBuild t m, DomBuilder t m, Enum a, Bounded a)
  => EnumInputConfig t m (Maybe a)
  -> m (DomInputEl t m (Maybe a))
selectInput cfg = do
  modAttrEv <- statusModAttrEv' cfg

  elClass "div" "input" $ do
    n <- elClass "div" "flex-row" $ do
      n' <- selectElement
        (  (def :: SelectElementConfig EventResult t (DomBuilderSpace m))
        &  selectElementConfig_initialValue
        .~ showNum (_inputConfig_initialValue cfg)
        &  selectElementConfig_setValue
        .~ fmap showNum (_inputConfig_setValue cfg)
        &  selectElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ _inputConfig_attributes cfg
        <> idAttr (_inputConfig_id cfg)
        &  selectElementConfig_elementConfig
        .  elementConfig_modifyAttributes
        .~ modAttrEv
        &  selectElementConfig_elementConfig
        .  elementConfig_eventSpec
        %~ _inputConfig_eventSpec cfg
        )
        (mapM_ mkOption (allPossible (_inputConfig_initialValue cfg)))

      selectIcon

      statusMessageIcon (_inputConfig_status cfg)
      pure n'

    statusMessageElement (_inputConfig_status cfg)

    pure $ InputEl
      { _inputEl_value    = parseEnum <$> _selectElement_value (fst n)
      , _inputEl_hasFocus = _selectElement_hasFocus (fst n)
      , _inputEl_elements = constDyn [_selectElement_element (fst n)]
      }
 where
  mkOption x = elAttr "option" ("value" =: showNum (Just x)) (toLabel x)
  toLabel = getOpElem (_inputConfig_extra cfg)

selectIcon :: (PostBuild t m, DomBuilder t m) => m ()
selectIcon = inputIcon arrowElement
 where
  arrowElement =
    icon def { _iconConfig_direction = constDyn DirDown } angleIcon


allPossible :: (Enum a, Bounded a) => f a -> [a]
allPossible _ = [minBound .. maxBound]

showNum :: Enum a => Maybe a -> Text
showNum (Just x) = pack . show . fromEnum $ x
showNum Nothing  = mempty

parseEnum :: Enum a => Text -> Maybe a
parseEnum = fmap toEnum . readMaybe . unpack

radioInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m, Ord a, Enum a, Bounded a)
  => EnumInputConfig t m (Maybe a)
  -> m (DomInputEl t m (Maybe a))
radioInput cfg = fmap Set.lookupMin <$> checkboxesInput
  (fmap (maybe Set.empty Set.singleton) cfg { _inputConfig_extra = Const () })
    { _inputConfig_attributes = _inputConfig_attributes cfg <> initAttrs
    , _inputConfig_extra      = OpElem (getOpElem (_inputConfig_extra cfg))
    }
 where
  initAttrs = "type" =: "radio" <> maybe mempty
                                         (Map.singleton "name")
                                         (_inputConfig_id cfg)

textAreaInput
  :: forall t m
   . (PostBuild t m, DomBuilder t m)
  => InputConfig t m Text
  -> m (DomInputEl t m Text)
textAreaInput cfg = do
  modAttrEv <- statusModAttrEv' cfg

  elClass "div" "input" $ do
    n <- elClass "div" "flex-row" $ do
      n' <- textAreaElement
        (  (def :: TextAreaElementConfig EventResult t (DomBuilderSpace m))
        &  textAreaElementConfig_initialValue
        .~ _inputConfig_initialValue cfg
        &  textAreaElementConfig_setValue
        .~ _inputConfig_setValue cfg
        &  textAreaElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ _inputConfig_attributes cfg
        <> idAttr (_inputConfig_id cfg)
        &  textAreaElementConfig_elementConfig
        .  elementConfig_modifyAttributes
        .~ mergeWith (<>) [modAttrEv, _inputConfig_modifyAttributes cfg]
        &  textAreaElementConfig_elementConfig
        .  elementConfig_eventSpec
        %~ _inputConfig_eventSpec cfg
        )

      statusMessageIcon (_inputConfig_status cfg)
      pure n'

    statusMessageElement (_inputConfig_status cfg)

    pure $ InputEl { _inputEl_value    = _textAreaElement_value n
                   , _inputEl_hasFocus = _textAreaElement_hasFocus n
                   , _inputEl_elements = constDyn [_textAreaElement_element n]
                   }

rangeInput
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , RealFloat a
     )
  => NumberInputConfig t m a
  -> m (DomInputEl t m (Maybe a))
rangeInput = numberRangeInput False

fileInput
  :: forall t m
   . (PostBuild t m, DomBuilder t m)
  => InputConfig t m ()
  -> m (DomInputEl t m [DOM.File])
fileInput cfg = do
  modAttrEv <- statusModAttrEv' cfg

  elClass "div" "input" $ do
    n <- elDynClass "label" (dynClass <$> _inputConfig_status cfg) $ do
      icon def folderIcon
      el "span" (text "Browse")
      inputElement
        $  (def :: InputElementConfig EventResult t (DomBuilderSpace m))
        &  inputElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ (_inputConfig_attributes cfg <> initAttrs)
        <> idAttr (_inputConfig_id cfg)
        &  inputElementConfig_elementConfig
        .  elementConfig_modifyAttributes
        .~ mergeWith (<>) [modAttrEv, _inputConfig_modifyAttributes cfg]
        &  inputElementConfig_elementConfig
        .  elementConfig_eventSpec
        %~ _inputConfig_eventSpec cfg

    elClass "div" "statusmessage" $ do
      statusMessageIcon (_inputConfig_status cfg)

      statusMessageElement (_inputConfig_status cfg)

    pure $ InputEl { _inputEl_value    = _inputElement_files n
                   , _inputEl_hasFocus = _inputElement_hasFocus n
                   , _inputEl_elements = constDyn [_inputElement_element n]
                   }

 where
  initAttrs = "type" =: "file" <> "multiple" =: ""
  dynClass x = "file-upload-label secondary"
    <> if x == InputDisabled then " disabled" else mempty

datalistInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, Ord k)
  => Dynamic t (Map k Text)
  -> InputConfig t m Text
  -> m (DomInputEl t m Text)
datalistInput options cfg = textInputWithIco
  after'
  cfg
    { _inputConfig_attributes = _inputConfig_attributes cfg
                                <> maybe
                                     mempty
                                     (Map.singleton "list" . (<> "-datalist"))
                                     (_inputConfig_id cfg)
                                <> "class"
                                =: "datalist"
    }
 where
  after' = do
    selectIcon

    _ <-
      elAttr
          "datalist"
          (maybe mempty
                 (Map.singleton "id" . (<> "-datalist"))
                 (_inputConfig_id cfg)
          )
        $ listWithKey options mkOption

    pure (never, never)

  mkOption _ v = elDynAttr "option" (Map.singleton "value" <$> v) (dynText v)

inputGroup
  :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m)
  => InputConfig t m ()
  -> m a
  -> m a
inputGroup cfg cnt = do
  modAttrEv <- statusModAttrEv (Just "input input-group")
                               (_inputConfig_status cfg)

  dynAttr <- holdDynAttributes
    (_inputConfig_attributes cfg <> initAttrs)
    (mergeWith (<>) [modAttrEv, _inputConfig_modifyAttributes cfg])

  elDynAttr "div" dynAttr $ do
    result <- elClass "div" "flex-row" $ do
      result' <- cnt

      statusMessageIcon (_inputConfig_status cfg)

      pure result'

    statusMessageElement (_inputConfig_status cfg)

    pure result

 where
  initAttrs = "class" =: "input input-group" <> idAttr (_inputConfig_id cfg)

holdDynAttributes
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Map AttributeName Text
  -> Event t (Map AttributeName (Maybe Text))
  -> m (Dynamic t (Map Text Text))
holdDynAttributes initAttrs modAttrEv = do
  dynAttr <- foldDyn updateAttrs initAttrs modAttrEv
  pure $ Map.mapKeys unNamespace <$> dynAttr
 where
  updateAttrs updates m = Map.foldrWithKey go m updates
   where
    go :: Ord k => k -> Maybe a -> Map k a -> Map k a
    go k ma = Map.alter (const ma) k

  unNamespace (AttributeName _ x) = x


passwordInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => InputConfig t m Text
  -> m (DomInputEl t m Text)
passwordInput cfg = textInputWithIco
  after'
  cfg
    { _inputConfig_attributes = _inputConfig_attributes cfg
                                <> "type"
                                =: "password"
    }
 where
  after' = do
    rec hideDyn    <- toggle True toggleEv
        toggleEvEv <- dyn (eyeIconEl <$> hideDyn)
        toggleEv   <- switchHold never toggleEvEv
    pure (mkAttrs <$> updated hideDyn, never)

  mkAttrs True  = "type" =: Just "password"
  mkAttrs False = "type" =: Just "text"

eyeIconEl :: (PostBuild t m, DomBuilder t m) => Bool -> m (Event t ())
eyeIconEl hide = do
  (e, _) <- elClass' "div" "input-icon" arrowElement
  pure $ domEvent Click e
 where
  arrowElement = icon def { _iconConfig_size = 5 / 4 }
                      (if hide then eyeIcon else eyeHideIcon)


timeOfDayInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => NumberInputConfig t m (Maybe TimeOfDay)
  -> m (DomInputEl t m (Maybe TimeOfDay))
timeOfDayInput = timeInput

timeInput
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , ParseTime a
     , FormatTime a
     , Ord a
     , MonadHold t m
     )
  => NumberInputConfig t m (Maybe a)
  -> m (DomInputEl t m (Maybe a))
timeInput cfg = datetimeInput formatTime' parseTime' "time" clockIcon cfg
 where
  formatTime' t = pack $ formatTime
    defaultTimeLocale
    (if maybe False (< 60) (_numberRange_precision nc) then "%S" else "%R")
    t

  nc = _inputConfig_extra cfg

  parseTime' x' =
    let x = unpack x'
    in  parseTimeM True defaultTimeLocale "%S" x
          <|> parseTimeM True defaultTimeLocale "%R" x

flattenNumberRange :: Reflex t => NumberRange t (Maybe a) -> NumberRange t a
flattenNumberRange cfg = cfg
  { _numberRange_minValue = join <$> _numberRange_minValue cfg
  , _numberRange_maxValue = join <$> _numberRange_maxValue cfg
  }

getMinMaxEv
  :: (PostBuild t m)
  => (a -> Text)
  -> NumberRange t a
  -> m
       ( Event t (Map AttributeName (Maybe Text))
       , Dynamic t (Maybe a, Maybe a)
       )
getMinMaxEv formatTime' nc = do
  let minMaxDyn = (,) <$> _numberRange_minValue nc <*> _numberRange_maxValue nc
  postBuildEv <-
    attachPromptlyDynWith (\x _ -> minMaxAttrs x) minMaxDyn <$> getPostBuild
  let updatedMinMaxEv = updated (minMaxAttrs <$> minMaxDyn)
  pure (mergeWith (<>) [postBuildEv, updatedMinMaxEv], minMaxDyn)
 where
  minMaxAttrs (b_min, b_max) =
    "min" =: fmap formatTime' b_min <> "max" =: fmap formatTime' b_max

datetimeInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, Ord a, MonadHold t m)
  => (a -> Text)
  -> (Text -> Maybe a)
  -> Text
  -> m ()
  -> NumberInputConfig t m (Maybe a)
  -> m (DomInputEl t m (Maybe a))
datetimeInput formatTime' parseTime' typeStr ico cfg = do

  (minMaxEv, minMaxDyn) <- getMinMaxEv formatTime' (flattenNumberRange nc)

  rec n <- textInputWithIco
        after'
        (fmap mFormatTime' cfg)
          { _inputConfig_status           = zipDynWith max
                                                       (_inputConfig_status cfg)
                                                       statusDyn
          , _inputConfig_attributes = _inputConfig_attributes cfg <> initAttrs
          , _inputConfig_modifyAttributes = mergeWith
                                              (<>)
                                              [ _inputConfig_modifyAttributes
                                                cfg
                                              , minMaxEv
                                              ]
          , _inputConfig_extra            = Const ()
          }
      let result0   = parseTime' <$> n
          result    = _inputEl_value result0
          modAttrEv = updated
            (   styleChange
            <$> minMaxDyn
            <*> result
            <*> _inputEl_hasFocus n
            <*> _inputEl_value n
            )
      statusDyn <- holdDyn def modAttrEv
  return result0
 where
  initAttrs = Map.fromList $ mapMaybe
    (\(x, y) -> (x, ) <$> y)
    [("type", Just typeStr), ("step", mkStep <$> _numberRange_precision nc)]

  styleChange (b_min, b_max) (Just x) _ _
    | maybe False (x >) b_max
    = InputError $ "Exceeds the maximum " <> formatTime' (fromJust b_max)
    | maybe False (x <) b_min
    = InputError $ "Is less than the minimum " <> formatTime' (fromJust b_min)
    | otherwise
    = def
  styleChange _ Nothing hasFocus t
    | Text.null t || hasFocus = def
    | -- Don't update the value when still typing
      otherwise               = InputError $ "Not a valid " <> typeStr

  nc = _inputConfig_extra cfg

  mkStep :: Int -> Text
  mkStep       = pack . show

  mFormatTime' = maybe mempty formatTime'

  after'       = do
    _ <- inputIcon (icon def ico)
    pure (never, never)


inputIcon :: DomBuilder t m => m () -> m ()
inputIcon = elClass "div" "input-icon nopointer"


dateInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => NumberInputConfig t m (Maybe Day)
  -> m (DomInputEl t m (Maybe Day))
dateInput = datetimeInput formatTime' parseTime' "date" calendarIcon
 where
  formatTime' = pack . formatTime defaultTimeLocale "%F"

  parseTime'  = parseTimeM True defaultTimeLocale "%F" . unpack

localtimeInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => NumberInputConfig t m (Maybe LocalTime)
  -> m (DomInputEl t m (Maybe LocalTime))
localtimeInput = datetimeInput formatTime'
                               parseTime'
                               "datetime-local"
                               clockIcon
 where
  formatTime' = pack . formatTime defaultTimeLocale "%FT%R"

  parseTime'  = parseTimeM True defaultTimeLocale "%FT%R" . unpack

weekInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => NumberInputConfig t m (Maybe (Integer, Int))
  -> m (DomInputEl t m (Maybe (Integer, Int)))
weekInput = datetimeInput formatTime' parseTime' "week" calendarIcon
 where
  formatTime' (y, m) =
    pack $ formatTime defaultTimeLocale "%Y-W%W" (fromWeekDate y m 1)

  parseTime' =
    fmap ((\(y, w, _) -> (y, w)) . toWeekDate)
      . parseTimeM True defaultTimeLocale "%Y-W%W"
      . unpack

monthInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => NumberInputConfig t m (Maybe (Integer, Int))
  -> m (DomInputEl t m (Maybe (Integer, Int)))
monthInput = datetimeInput formatTime' parseTime' "month" calendarIcon
 where
  formatTime' (y, m) =
    pack $ formatTime defaultTimeLocale "%Y-%m" (fromGregorian y m 1)

  parseTime' =
    fmap ((\(y, m, _) -> (y, m)) . toGregorian)
      . parseTimeM True defaultTimeLocale "%Y-%m"
      . unpack

requiredInput
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (InputConfig' c t m (Maybe a) -> m (DomInputEl t m (Maybe a)))
  -> InputConfig' c t m (Maybe a)
  -> m (DomInputEl t m (Maybe a))
requiredInput fn cfg = do
  rec r <- fn cfg
        { _inputConfig_attributes = _inputConfig_attributes cfg
                                    <> "required"
                                    =: ""
        , _inputConfig_status     = _inputConfig_status cfg <> stat
        }
      lostFocusDyn <- lostFocus (_inputEl_hasFocus r)
      let stat = mkStat <$> lostFocusDyn <*> _inputEl_value r
  pure r
 where
  mkStat True Nothing = InputError "This field is required."
  mkStat _    _       = def

-- | Turns true if the input goes from true to false
lostFocus :: (MonadHold t m, Reflex t) => Dynamic t Bool -> m (Dynamic t Bool)
lostFocus x = do
  hadFocusDyn <- holdDyn False (ffilter Prelude.id (updated x))
  let lostFocusEv = ffilter not (updated x)
  holdDyn False (True <$ gate (current hadFocusDyn) lostFocusEv)

