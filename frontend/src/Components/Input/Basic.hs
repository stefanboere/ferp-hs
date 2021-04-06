{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Components.Input.Basic
  ( numberInput
  , numberInput'
  , integralInput
  , integralInput'
  , toggleInput
  , togglesInput
  , checkboxInput
  , checkboxesInput
  , checkboxesInputMap
  , checkboxesInputMap'
  , checkboxesInputDynMap
  , checkboxesInputDynMap'
  , checkboxInputSimple
  , inputStyle
  , InputConfig(..)
  , inputConfig
  , textInput
  , textInput'
  , textInputWithIco
  , textInputWithIco'
  , InputStatus(..)
  , InputEl(..)
  , labeled
  , randomId
  , NumberInputConfig(..)
  , HasLabel(..)
  , selectInput'
  , selectInput
  , textAreaInput
  , textAreaInput'
  , rangeInput
  , rangeInput'
  , radioInput
  , radioInput'
  , fileInput
  , fileInput'
  , datalistInput
  , datalistInput'
  , selectIcon
  , inputGroup
  , inputGroup'
  , passwordInput
  , passwordInput'
  , timeInput
  , timeInput'
  , dateInput
  , dateInput'
  , localtimeInput
  , localtimeInput'
  , weekInput
  , weekInput'
  , monthInput
  , monthInput'
  , statusModAttrEv'
  , statusMessageDiv
  )
where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( (&)
                                                , icon
                                                , max
                                                , not
                                                , selectElement
                                                , empty
                                                )
import qualified Clay                           ( (&) )
import qualified Clay.Media                    as Media
import           Clay.Stylesheet                ( key )
import           Control.Applicative            ( (<|>) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Default
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Time
import           Data.Time.Calendar.WeekDate
import qualified GHCJS.DOM.Types               as DOM
                                                ( File )
import           Numeric                        ( showFFloatAlt )
import           Reflex
import           Reflex.Dom              hiding ( textInput
                                                , rangeInput
                                                , fileInput
                                                )
import           Text.Read                      ( readMaybe )
import           Text.Printf                    ( printf )
import           System.Random                  ( getStdGen
                                                , StdGen
                                                , setStdGen
                                                , random
                                                )

import           Components.Class
import           Components.Icon
import           Nordtheme

data InputStatus = InputNeutral (Maybe Text) | InputSuccess Text | InputError Text | InputDisabled deriving (Eq, Ord)

instance Default InputStatus where
  def = InputNeutral Nothing

instance Semigroup InputStatus where
  (InputNeutral _) <> x = x
  x                <> _ = x

data InputConfig t a = InputConfig
  { _inputConfig_initialValue :: a
  , _inputConfig_setValue     :: Event t a
  , _inputConfig_label        :: Dynamic t Text
  , _inputConfig_status       :: Dynamic t InputStatus
  , _inputConfig_attributes   :: Map AttributeName Text
  , _inputConfig_modifyAttributes :: Event t (Map AttributeName (Maybe Text))
  }

data InputEl d t a = InputEl
  { _inputEl_value :: Dynamic t a
  , _inputEl_hasFocus :: Dynamic t Bool
  , _inputEl_element :: Element EventResult d t
  }

instance Reflex t => Functor (InputEl d t) where
  fmap f x = x { _inputEl_value = f <$> _inputEl_value x }

inputConfig :: Reflex t => a -> InputConfig t a
inputConfig initval = InputConfig { _inputConfig_initialValue     = initval
                                  , _inputConfig_setValue         = never
                                  , _inputConfig_label            = constDyn ""
                                  , _inputConfig_status           = def
                                  , _inputConfig_attributes       = def
                                  , _inputConfig_modifyAttributes = never
                                  }

instance (Default a, Reflex t) => Default (InputConfig t a) where
  def = inputConfig def

instance Reflex t => Functor (InputConfig t) where
  fmap f cfg = cfg
    { _inputConfig_initialValue = f $ _inputConfig_initialValue cfg
    , _inputConfig_setValue     = f <$> _inputConfig_setValue cfg
    }

data NumberInputConfig t a = NumberInputConfig
  { _numberInputConfig_maxValue :: Dynamic t (Maybe a)
  , _numberInputConfig_minValue :: Dynamic t (Maybe a)
  , _numberInputConfig_precision :: Maybe Int
  }

instance Reflex t => Default (NumberInputConfig t a) where
  def = NumberInputConfig def def Nothing

instance Reflex t => Functor (NumberInputConfig t) where
  fmap f cfg = cfg
    { _numberInputConfig_maxValue = fmap f <$> _numberInputConfig_maxValue cfg
    , _numberInputConfig_minValue = fmap f <$> _numberInputConfig_minValue cfg
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
    left (rem (-0.25))
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
    width (rem 0.9)
    height (rem 0.9)
    left (rem (-0.15))
    top (rem 0.1)
    borderRadiusAll (pct 50)
    background white0'
    transitionDuration 0.1
    transitionTimingFunction easeIn

    checked Clay.& do
      left (rem 0.9)
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
        position relative
        cursor pointer
        marginRight (rem 0.8)

        disabled Clay.& cursor notAllowed

        before Clay.& do
          absoluteBlock
          width (rem 1)
          height (rem 1)
          borderRadiusAll (px 3)
          border solid 1 grey0'
          backgroundColor white0'
          ".has-error" Clay.& borderColor nord11'

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
          top (rem 0.15)
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
    top (rem 0.35)
    background white0'
    borderRadiusAll (pct 50)

    checked Clay.& absoluteBlock


inputElementStyle :: Css
inputElementStyle = (input <> Clay.select <> textarea <> ".dropzone") ? do
  background transparent
  borderWidth 0
  borderBottomWidth 1
  paddingAll (rem 0.25)
  borderColor grey0'
  outlineWidth 0
  flexGrow 1
  fontColor inherit
  "font" -: "inherit"

  focus Clay.& do
    borderBottomWidth 2
    borderColor nord10'
    marginBottom (px (-1))

  "::placeholder" Clay.& fontColor grey0'

  disabled Clay.& do
    fontColor grey0'
    cursor notAllowed

  ".has-error" Clay.& borderColor nord11'

  ".has-success" Clay.& borderColor green1'

selectElementStyle :: Css
selectElementStyle = do
  Clay.select ? do
    paddingRight (rem (3 / 2))
    cursor pointer
    flexGrow 1
    "appearance" -: "none"
    "-webkit-appearance" -: "none"
    "-moz-appearance" -: "none"

  (input <> Clay.select) |+ ".input-icon" ? do
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
  minWidth (rem 20)
  minHeight (rem 6)

  disabled Clay.& do
    background white0'
    cursor notAllowed

rangeElementStyle :: Css
rangeElementStyle = input # ("type" @= "range") ? do
  "-webkit-appearance" -: "none"
  paddingAll nil
  height (Clay.rem 0.2)
  borderRadiusAll (px 3)
  cursor pointer
  background nord10'
  disabledStyle
  marginTop (rem (2 / 3))

  "::-webkit-slider-thumb" Clay.& do
    "-webkit-appearance" -: "none"
    thumb

  "::-moz-range-thumb" Clay.& thumb

  focus Clay.& do
    borderBottomWidth nil
    marginBottom (px 2)

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
    (Clay.select <> textarea <> input <> ".drozone") ? borderColor nord11'

  ".has-success" Clay.& do
    (Clay.select <> textarea <> input <> ".dropzone") ? borderColor green1'

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
  => InputConfig t a
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
  => InputConfig t a
  -> (Text -> InputConfig t a -> m b)
  -> m b
labeled cfg editor = do
  idStr <- labelFor (_inputConfig_label cfg)
  editor idStr cfg

textInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadIO m)
  => InputConfig t Text
  -> m (InputEl (DomBuilderSpace m) t Text)
textInput cfg = labeled cfg textInput'

textInput'
  :: (PostBuild t m, DomBuilder t m, MonadFix m)
  => Text
  -> InputConfig t Text
  -> m (InputEl (DomBuilderSpace m) t Text)
textInput' = textInputWithIco (pure (never, never))

textInputWithIco
  :: (PostBuild t m, DomBuilder t m, MonadFix m)
  => m (Event t (Map AttributeName (Maybe Text)), Event t Text)
  -> Text
  -> InputConfig t Text
  -> m (InputEl (DomBuilderSpace m) t Text)
textInputWithIco after' idStr cfg =
  fst <$> textInputWithIco' (((), ) <$> after') idStr cfg

textInputWithIco'
  :: (PostBuild t m, DomBuilder t m, MonadFix m)
  => m (a, (Event t (Map AttributeName (Maybe Text)), Event t Text))
  -> Text
  -> InputConfig t Text
  -> m (InputEl (DomBuilderSpace m) t Text, a)
textInputWithIco' after' idStr cfg = do
  modAttrEv <- statusModAttrEv' cfg

  elClass "div" "input" $ do
    (n, x) <- elClass "div" "flex-row" $ do
      rec
        n' <-
          inputElement
          $  def
          &  inputElementConfig_initialValue
          .~ _inputConfig_initialValue cfg
          &  inputElementConfig_setValue
          .~ leftmost [setValEv, _inputConfig_setValue cfg]
          &  inputElementConfig_elementConfig
          .  elementConfig_initialAttributes
          .~ _inputConfig_attributes cfg
          <> "id"
          =: idStr
          &  inputElementConfig_elementConfig
          .  elementConfig_modifyAttributes
          .~ mergeWith (<>)
                       [modAttrEv, _inputConfig_modifyAttributes cfg, attrEv]

        (x, (attrEv, setValEv)) <- after'

      statusMessageIcon (_inputConfig_status cfg)
      pure (n', x)

    statusMessageElement (_inputConfig_status cfg)

    pure
      ( InputEl { _inputEl_value    = _inputElement_value n
                , _inputEl_hasFocus = _inputElement_hasFocus n
                , _inputEl_element  = _inputElement_element n
                }
      , x
      )

numberInput
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , RealFloat a
     , MonadIO m
     )
  => NumberInputConfig t a
  -> InputConfig t a
  -> m (Dynamic t (Maybe a))
numberInput nc cfg = labeled cfg (numberInput' nc)

numberInput'
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , RealFloat a
     )
  => NumberInputConfig t a
  -> Text
  -> InputConfig t a
  -> m (Dynamic t (Maybe a))
numberInput' = numberRangeInput' True

integralInput
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Integral a
     , MonadIO m
     )
  => NumberInputConfig t a
  -> InputConfig t a
  -> m (Dynamic t (Maybe a))
integralInput nc cfg = labeled cfg (integralInput' nc)

integralInput'
  :: (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m, Integral a)
  => NumberInputConfig t a
  -> Text
  -> InputConfig t a
  -> m (Dynamic t (Maybe a))
integralInput' nc idStr cfg = conv <$> numberInput'
  (fromIntegral <$> nc { _numberInputConfig_precision = Just 0 })
  idStr
  (fromIntegral <$> cfg)
 where
  conv
    :: (Integral a, Reflex t) => Dynamic t (Maybe Double) -> Dynamic t (Maybe a)
  conv = fmap (fmap Prelude.round)

numberRangeInput'
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , RealFloat a
     )
  => Bool
  -> NumberInputConfig t a
  -> Text
  -> InputConfig t a
  -> m (Dynamic t (Maybe a))
numberRangeInput' isReg nc idStr cfg = do
  let
    initAttrs = Map.fromList $ mapMaybe
      (\(x, y) -> (x, ) <$> y)
      [ ("type" , if isReg then Just "number" else Just "range")
      , ("style", Just "text-align:right")
      , ( "onClick"
        , if _inputConfig_initialValue cfg == 0 && isReg
          then Just "this.select()"
          else Nothing
        )
      , ("step", mkStep <$> _numberInputConfig_precision nc)
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

  (minMaxEv, minMaxDyn) <- getMinMaxEv prnt nc

  rec
    n <- textInput'
      idStr
      cfg
        { _inputConfig_initialValue     = prnt $ _inputConfig_initialValue cfg
        , _inputConfig_setValue         = leftmost
          [prnt <$> _inputConfig_setValue cfg, zeroIfEmptyEv]
        , _inputConfig_status           = zipDynWith max
                                                     (_inputConfig_status cfg)
                                                     statusDyn
        , _inputConfig_attributes = _inputConfig_attributes cfg <> initAttrs
        , _inputConfig_modifyAttributes = mergeWith
                                            (<>)
                                            [ _inputConfig_modifyAttributes cfg
                                            , minMaxEv
                                            , selectAttrEv
                                            ]
        }
    let result        = readMaybe . unpack <$> _inputEl_value n
        selectAttrEv  = if isReg then mkOnClick <$> updated result else never
        zeroIfEmptyEv = attachPromptlyDynWithMaybe
          emptyNoFocus
          ((,) <$> _inputEl_value n <*> result)
          (updated (_inputEl_hasFocus n))
        modAttrEv = updated
          (   styleChange
          <$> minMaxDyn
          <*> result
          <*> _inputEl_hasFocus n
          <*> _inputEl_value n
          )
    statusDyn <- holdDyn def modAttrEv
  return result
 where
  mkOnClick x = "onClick"
    =: if isNothing x || x == Just 0 then Just "this.select()" else Nothing

  mkStep :: Int -> Text
  mkStep x = prnt (10 ^^ (-x))
  prnt x = Text.dropWhileEnd (== '.') $ pack $ showFFloatAlt
    (_numberInputConfig_precision nc)
    x
    ""
  emptyNoFocus (x, r) f | Text.null x && not f = Just (prnt 0)
                        | not f                = prnt <$> r
                        | otherwise            = Nothing

toggleInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => Text
  -> InputConfig t Bool
  -> m (Dynamic t Bool)
toggleInput lbl cfg = do
  let initAttrs = "class" =: "toggle"

  checkboxInput
    lbl
    cfg { _inputConfig_attributes = _inputConfig_attributes cfg <> initAttrs }

togglesInput
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadIO m
     , Ord a
     , HasLabel a
     , Enum a
     , Bounded a
     )
  => InputConfig t (Set a)
  -> m (Dynamic t (Set a))
togglesInput cfg = do
  let initAttrs = "class" =: "toggle"

  checkboxesInput cfg
    { _inputConfig_attributes = _inputConfig_attributes cfg <> initAttrs
    }

checkboxesInput
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadIO m
     , Ord a
     , HasLabel a
     , Enum a
     , Bounded a
     )
  => InputConfig t (Set a)
  -> m (Dynamic t (Set a))
checkboxesInput cfg =
  checkboxesInputMap (allPossibleMap (_inputConfig_initialValue cfg)) cfg

checkboxesInput'
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadIO m
     , Ord a
     , HasLabel a
     , Enum a
     , Bounded a
     )
  => Text
  -> InputConfig t (Set a)
  -> m (Dynamic t (Set a))
checkboxesInput' idStr cfg =
  checkboxesInputMap' (allPossibleMap (_inputConfig_initialValue cfg)) idStr cfg

allPossibleMap :: (HasLabel a, Bounded a, Enum a, Ord a) => f a -> Map a Text
allPossibleMap = Map.fromList . fmap (\x -> (x, toLabel x)) . allPossible

-- | Same as 'checkboxesInput' but with a map containing all options
checkboxesInputMap
  :: (PostBuild t m, DomBuilder t m, MonadIO m, Ord a)
  => Map a Text
  -> InputConfig t (Set a)
  -> m (Dynamic t (Set a))
checkboxesInputMap opts cfg = labeled cfg (checkboxesInputMap' opts)

checkboxesInputMap'
  :: (PostBuild t m, DomBuilder t m, MonadIO m, Ord a)
  => Map a Text
  -> Text
  -> InputConfig t (Set a)
  -> m (Dynamic t (Set a))
checkboxesInputMap' opts idStr' cfg =
  elAttr "div" ("class" =: "input" <> "id" =: idStr') $ do
    modAttrEv <- statusModAttrEv' cfg

    result    <- Map.traverseWithKey (mkCheckbox modAttrEv) opts

    statusMessageDiv (_inputConfig_status cfg)

    pure
      (Map.keysSet . Map.filter Prelude.id <$> joinDynThroughMap
        (constDyn result)
      )
 where
  mkCheckbox modAttrEv k x =
    let cfg' = ((k `elem`) <$> cfg)
    in  checkboxInput' cfg'
          { _inputConfig_modifyAttributes = mergeWith
                                              (<>)
                                              [ modAttrEv
                                              , _inputConfig_modifyAttributes
                                                cfg'
                                              ]
          , _inputConfig_label            = constDyn x
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
  => Dynamic t (Map a Text)
  -> InputConfig t (Set a)
  -> m (Dynamic t (Set a))
checkboxesInputDynMap opts cfg = labeled cfg (checkboxesInputDynMap' opts)

checkboxesInputDynMap'
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadIO m
     , MonadHold t m
     , MonadFix m
     , Ord a
     )
  => Dynamic t (Map a Text)
  -> Text
  -> InputConfig t (Set a)
  -> m (Dynamic t (Set a))
checkboxesInputDynMap' opts idStr' cfg =
  elAttr "div" ("class" =: "input" <> "id" =: idStr') $ do
    modAttrEv <- statusModAttrEv' cfg

    result    <- listWithKey opts (mkCheckbox modAttrEv)

    statusMessageDiv (_inputConfig_status cfg)

    pure (Map.keysSet . Map.filter Prelude.id <$> joinDynThroughMap result)
 where
  mkCheckbox modAttrEv k x =
    let cfg' = ((k `elem`) <$> cfg)
    in  checkboxInput' cfg'
          { _inputConfig_modifyAttributes = mergeWith
                                              (<>)
                                              [ modAttrEv
                                              , _inputConfig_modifyAttributes
                                                cfg'
                                              ]
          , _inputConfig_label            = x
          }

statusMessageDiv
  :: (PostBuild t m, DomBuilder t m) => Dynamic t InputStatus -> m ()
statusMessageDiv status = elClass "div" "statusmessage" $ do
  statusMessageIcon status
  statusMessageElement status

checkboxInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => Text
  -> InputConfig t Bool
  -> m (Dynamic t Bool)
checkboxInput lbl cfg = fmap (Prelude.not . Set.null) <$> checkboxesInputMap
  (Map.singleton () lbl)
  ((\x -> if x then Set.singleton () else Set.empty) <$> cfg)

checkboxInput'
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => InputConfig t Bool
  -> m (Dynamic t Bool)
checkboxInput' cfg = el "div" $ do
  idStr <- randomId
  n     <-
    inputElement
    $  def
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

  elAttr "label" ("for" =: idStr <> "class" =: "checkbox-label")
    $ dynText
    $ _inputConfig_label cfg

  pure (_inputElement_checked n)

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

class HasLabel a where
  toLabel :: a -> Text

selectInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m, HasLabel a, Enum a, Bounded a)
  => InputConfig t (Maybe a)
  -> m (Dynamic t (Maybe a))
selectInput cfg = _inputEl_value <$> labeled cfg selectInput'

selectInput'
  :: (PostBuild t m, DomBuilder t m, HasLabel a, Enum a, Bounded a)
  => Text
  -> InputConfig t (Maybe a)
  -> m (InputEl (DomBuilderSpace m) t (Maybe a))
selectInput' idStr cfg = do
  modAttrEv <- statusModAttrEv' cfg

  elClass "div" "input" $ do
    n <- elClass "div" "flex-row" $ do
      n' <- selectElement
        (  def
        &  selectElementConfig_initialValue
        .~ showNum (_inputConfig_initialValue cfg)
        &  selectElementConfig_setValue
        .~ fmap showNum (_inputConfig_setValue cfg)
        &  selectElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ _inputConfig_attributes cfg
        <> "id"
        =: idStr
        &  selectElementConfig_elementConfig
        .  elementConfig_modifyAttributes
        .~ modAttrEv
        )
        (mapM_ mkOption (allPossible (_inputConfig_initialValue cfg)))

      selectIcon

      statusMessageIcon (_inputConfig_status cfg)
      pure n'

    statusMessageElement (_inputConfig_status cfg)

    pure $ InputEl { _inputEl_value = parseEnum <$> _selectElement_value (fst n)
                   , _inputEl_hasFocus = _selectElement_hasFocus (fst n)
                   , _inputEl_element = _selectElement_element (fst n)
                   }
 where
  mkOption x = elAttr "option" ("value" =: showNum (Just x)) (text (toLabel x))

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
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadIO m
     , Ord a
     , HasLabel a
     , Enum a
     , Bounded a
     )
  => InputConfig t (Maybe a)
  -> m (Dynamic t (Maybe a))
radioInput cfg = labeled cfg radioInput'

radioInput'
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadIO m
     , Ord a
     , HasLabel a
     , Enum a
     , Bounded a
     )
  => Text
  -> InputConfig t (Maybe a)
  -> m (Dynamic t (Maybe a))
radioInput' idStr cfg = fmap Set.lookupMin <$> checkboxesInput'
  idStr
  (fmap (maybe Set.empty Set.singleton) cfg)
    { _inputConfig_attributes = _inputConfig_attributes cfg <> initAttrs
    }
  where initAttrs = "type" =: "radio" <> "name" =: idStr

textAreaInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => InputConfig t Text
  -> m (Dynamic t Text)
textAreaInput cfg = _inputEl_value <$> labeled cfg textAreaInput'

textAreaInput'
  :: (PostBuild t m, DomBuilder t m)
  => Text
  -> InputConfig t Text
  -> m (InputEl (DomBuilderSpace m) t Text)
textAreaInput' idStr cfg = do
  modAttrEv <- statusModAttrEv' cfg

  elClass "div" "input" $ do
    n <- elClass "div" "flex-row" $ do
      n' <- textAreaElement
        (  def
        &  textAreaElementConfig_initialValue
        .~ _inputConfig_initialValue cfg
        &  textAreaElementConfig_setValue
        .~ _inputConfig_setValue cfg
        &  textAreaElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ _inputConfig_attributes cfg
        <> "id"
        =: idStr
        &  textAreaElementConfig_elementConfig
        .  elementConfig_modifyAttributes
        .~ mergeWith (<>) [modAttrEv, _inputConfig_modifyAttributes cfg]
        )

      statusMessageIcon (_inputConfig_status cfg)
      pure n'

    statusMessageElement (_inputConfig_status cfg)

    pure $ InputEl { _inputEl_value    = _textAreaElement_value n
                   , _inputEl_hasFocus = _textAreaElement_hasFocus n
                   , _inputEl_element  = _textAreaElement_element n
                   }

rangeInput
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , RealFloat a
     , MonadIO m
     )
  => NumberInputConfig t a
  -> InputConfig t a
  -> m (Dynamic t (Maybe a))
rangeInput nc cfg = labeled cfg (rangeInput' nc)

rangeInput'
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , RealFloat a
     )
  => NumberInputConfig t a
  -> Text
  -> InputConfig t a
  -> m (Dynamic t (Maybe a))
rangeInput' = numberRangeInput' False

fileInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => InputConfig t ()
  -> m (Dynamic t [DOM.File])
fileInput cfg = labeled cfg fileInput'

fileInput'
  :: (PostBuild t m, DomBuilder t m)
  => Text
  -> InputConfig t ()
  -> m (Dynamic t [DOM.File])
fileInput' idStr cfg = do
  modAttrEv <- statusModAttrEv' cfg

  elClass "div" "input" $ do
    n <- elDynClass "label" (dynClass <$> _inputConfig_status cfg) $ do
      icon def folderIcon
      el "span" (text "Browse")
      inputElement
        $  def
        &  inputElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ (_inputConfig_attributes cfg <> initAttrs)
        <> "id"
        =: idStr
        &  inputElementConfig_elementConfig
        .  elementConfig_modifyAttributes
        .~ mergeWith (<>) [modAttrEv, _inputConfig_modifyAttributes cfg]

    elClass "div" "statusmessage" $ do
      statusMessageIcon (_inputConfig_status cfg)

      statusMessageElement (_inputConfig_status cfg)

    pure $ _inputElement_files n

 where
  initAttrs = "type" =: "file" <> "multiple" =: ""
  dynClass x = "file-upload-label secondary"
    <> if x == InputDisabled then " disabled" else mempty

datalistInput
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , MonadIO m
     , Ord k
     , Show k
     )
  => Dynamic t (Map k Text)
  -> InputConfig t Text
  -> m (Dynamic t Text)
datalistInput options cfg =
  _inputEl_value <$> labeled cfg (`datalistInput'` options)

datalistInput'
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, Ord k, Show k)
  => Text
  -> Dynamic t (Map k Text)
  -> InputConfig t Text
  -> m (InputEl (DomBuilderSpace m) t Text)
datalistInput' idStr options cfg = textInputWithIco
  after'
  idStr
  cfg
    { _inputConfig_attributes = _inputConfig_attributes cfg
                                <> "list"
                                =: listIdStr
                                <> "class"
                                =: "datalist"
    }
 where
  listIdStr = idStr <> "-datalist"
  after'    = do
    selectIcon

    _ <- elAttr "datalist" ("id" =: listIdStr) $ listWithKey options mkOption

    pure (never, never)

  mkOption k v = elAttr "option" ("value" =: pack (show k)) (dynText v)

inputGroup
  :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m, MonadIO m)
  => InputConfig t ()
  -> m a
  -> m a
inputGroup cfg cnt = labeled cfg (\idStr _ -> inputGroup' idStr cfg cnt)

inputGroup'
  :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m)
  => Text
  -> InputConfig t ()
  -> m a
  -> m a
inputGroup' idStr cfg cnt = do
  modAttrEv <- statusModAttrEv (Just "input input-group")
                               (_inputConfig_status cfg)

  dynAttr <- foldDyn
    updateAttrs
    (_inputConfig_attributes cfg <> initAttrs)
    (mergeWith (<>) [modAttrEv, _inputConfig_modifyAttributes cfg])

  elDynAttr "div" (Map.mapKeys unNamespace <$> dynAttr) $ do
    result <- elClass "div" "flex-row" $ do
      result' <- cnt

      statusMessageIcon (_inputConfig_status cfg)

      pure result'

    statusMessageElement (_inputConfig_status cfg)

    pure result

 where
  initAttrs = "class" =: "input input-group" <> "id" =: idStr

  unNamespace (AttributeName _ x) = x

  updateAttrs updates m = Map.foldrWithKey go m updates
   where
    go :: Ord k => k -> Maybe a -> Map k a -> Map k a
    go k ma = Map.alter (const ma) k


passwordInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, MonadIO m)
  => InputConfig t Text
  -> m (Dynamic t Text)
passwordInput cfg = _inputEl_value <$> labeled cfg passwordInput'

passwordInput'
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => Text
  -> InputConfig t Text
  -> m (InputEl (DomBuilderSpace m) t Text)
passwordInput' idStr cfg = textInputWithIco
  after'
  idStr
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


timeInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadIO m, MonadHold t m)
  => NumberInputConfig t TimeOfDay
  -> InputConfig t (Maybe TimeOfDay)
  -> m (Dynamic t (Maybe TimeOfDay))
timeInput nc cfg = labeled cfg (timeInput' nc)

timeInput'
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , ParseTime a
     , FormatTime a
     , Ord a
     , MonadHold t m
     )
  => NumberInputConfig t a
  -> Text
  -> InputConfig t (Maybe a)
  -> m (Dynamic t (Maybe a))
timeInput' nc = datetimeInput' formatTime' parseTime' "time" clockIcon nc
 where
  formatTime' t = pack $ formatTime
    defaultTimeLocale
    (if maybe False (< 60) (_numberInputConfig_precision nc) then "%S" else "%R"
    )
    t

  parseTime' x' =
    let x = unpack x'
    in  parseTimeM True defaultTimeLocale "%S" x
          <|> parseTimeM True defaultTimeLocale "%R" x

getMinMaxEv
  :: (PostBuild t m)
  => (a -> Text)
  -> NumberInputConfig t a
  -> m
       ( Event t (Map AttributeName (Maybe Text))
       , Dynamic t (Maybe a, Maybe a)
       )
getMinMaxEv formatTime' nc = do
  let minMaxDyn =
        (,)
          <$> _numberInputConfig_minValue nc
          <*> _numberInputConfig_maxValue nc
  postBuildEv <-
    attachPromptlyDynWith (\x _ -> minMaxAttrs x) minMaxDyn <$> getPostBuild
  let updatedMinMaxEv = updated (minMaxAttrs <$> minMaxDyn)
  pure (mergeWith (<>) [postBuildEv, updatedMinMaxEv], minMaxDyn)
 where
  minMaxAttrs (b_min, b_max) =
    "min" =: fmap formatTime' b_min <> "max" =: fmap formatTime' b_max

datetimeInput'
  :: (PostBuild t m, DomBuilder t m, MonadFix m, Ord a, MonadHold t m)
  => (a -> Text)
  -> (Text -> Maybe a)
  -> Text
  -> m ()
  -> NumberInputConfig t a
  -> Text
  -> InputConfig t (Maybe a)
  -> m (Dynamic t (Maybe a))
datetimeInput' formatTime' parseTime' typeStr ico nc idStr cfg = do

  (minMaxEv, minMaxDyn) <- getMinMaxEv formatTime' nc

  rec n <- textInputWithIco
        after'
        idStr
        (fmap mFormatTime' cfg)
          { _inputConfig_status = zipDynWith max
                                             (_inputConfig_status cfg)
                                             statusDyn
          , _inputConfig_attributes = _inputConfig_attributes cfg <> initAttrs
          , _inputConfig_modifyAttributes = mergeWith
                                              (<>)
                                              [ _inputConfig_modifyAttributes
                                                cfg
                                              , minMaxEv
                                              ]
          }
      let result    = parseTime' <$> _inputEl_value n
          modAttrEv = updated
            (   styleChange
            <$> minMaxDyn
            <*> result
            <*> _inputEl_hasFocus n
            <*> _inputEl_value n
            )
      statusDyn <- holdDyn def modAttrEv
  return result
 where
  initAttrs = Map.fromList $ mapMaybe
    (\(x, y) -> (x, ) <$> y)
    [ ("type", Just typeStr)
    , ("step", mkStep <$> _numberInputConfig_precision nc)
    ]

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


  mkStep :: Int -> Text
  mkStep       = pack . show

  mFormatTime' = maybe mempty formatTime'

  after'       = do
    _ <- inputIcon ico
    pure (never, never)


inputIcon :: (PostBuild t m, DomBuilder t m) => m () -> m ()
inputIcon ico = elClass "div" "input-icon nopointer" arrowElement
  where arrowElement = icon def ico


dateInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadIO m, MonadHold t m)
  => NumberInputConfig t Day
  -> InputConfig t (Maybe Day)
  -> m (Dynamic t (Maybe Day))
dateInput nc cfg = labeled cfg (dateInput' nc)

dateInput'
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => NumberInputConfig t Day
  -> Text
  -> InputConfig t (Maybe Day)
  -> m (Dynamic t (Maybe Day))
dateInput' = datetimeInput' formatTime' parseTime' "date" calendarIcon
 where
  formatTime' = pack . formatTime defaultTimeLocale "%F"

  parseTime'  = parseTimeM True defaultTimeLocale "%F" . unpack

localtimeInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadIO m, MonadHold t m)
  => NumberInputConfig t LocalTime
  -> InputConfig t (Maybe LocalTime)
  -> m (Dynamic t (Maybe LocalTime))
localtimeInput nc cfg = labeled cfg (localtimeInput' nc)

localtimeInput'
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => NumberInputConfig t LocalTime
  -> Text
  -> InputConfig t (Maybe LocalTime)
  -> m (Dynamic t (Maybe LocalTime))
localtimeInput' = datetimeInput' formatTime'
                                 parseTime'
                                 "datetime-local"
                                 clockIcon
 where
  formatTime' = pack . formatTime defaultTimeLocale "%FT%R"

  parseTime'  = parseTimeM True defaultTimeLocale "%FT%R" . unpack

weekInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadIO m, MonadHold t m)
  => NumberInputConfig t (Integer, Int)
  -> InputConfig t (Maybe (Integer, Int))
  -> m (Dynamic t (Maybe (Integer, Int)))
weekInput nc cfg = labeled cfg (weekInput' nc)

weekInput'
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => NumberInputConfig t (Integer, Int)
  -> Text
  -> InputConfig t (Maybe (Integer, Int))
  -> m (Dynamic t (Maybe (Integer, Int)))
weekInput' = datetimeInput' formatTime' parseTime' "week" calendarIcon
 where
  formatTime' (y, m) =
    pack $ formatTime defaultTimeLocale "%Y-W%W" (fromWeekDate y m 1)

  parseTime' =
    fmap ((\(y, w, _) -> (y, w)) . toWeekDate)
      . parseTimeM True defaultTimeLocale "%Y-W%W"
      . unpack

monthInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadIO m, MonadHold t m)
  => NumberInputConfig t (Integer, Int)
  -> InputConfig t (Maybe (Integer, Int))
  -> m (Dynamic t (Maybe (Integer, Int)))
monthInput nc cfg = labeled cfg (monthInput' nc)

monthInput'
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => NumberInputConfig t (Integer, Int)
  -> Text
  -> InputConfig t (Maybe (Integer, Int))
  -> m (Dynamic t (Maybe (Integer, Int)))
monthInput' = datetimeInput' formatTime' parseTime' "month" calendarIcon
 where
  formatTime' (y, m) =
    pack $ formatTime defaultTimeLocale "%Y-%m" (fromGregorian y m 1)

  parseTime' =
    fmap ((\(y, m, _) -> (y, m)) . toGregorian)
      . parseTimeM True defaultTimeLocale "%Y-%m"
      . unpack
