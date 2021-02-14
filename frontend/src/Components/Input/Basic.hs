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
  , toggleInput
  , checkboxInput
  , inputStyle
  , InputConfig(..)
  , textInput
  , textInput'
  , InputStatus(..)
  , labeled
  , NumberInputConfig(..)
  , HasLabel(..)
  , selectInput'
  , selectInput
  , textAreaInput
  , textAreaInput'
  )
where

import           Clay                    hiding ( (&)
                                                , icon
                                                , max
                                                , not
                                                , selectElement
                                                )
import qualified Clay                           ( (&) )
import           Clay.Stylesheet                ( key )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Default
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Numeric                        ( showFFloatAlt )
import           Reflex
import           Reflex.Dom              hiding ( textInput )
import           Text.Read                      ( readMaybe )
import           Text.Printf                    ( printf )
import           System.Random                  ( getStdGen
                                                , StdGen
                                                , setStdGen
                                                , random
                                                )

import           Components.Icon
import           Nordtheme

data InputStatus = InputNeutral (Maybe Text) | InputSuccess Text | InputError Text | InputDisabled deriving (Eq, Ord)

instance Default InputStatus where
  def = InputNeutral Nothing

data InputConfig t a = InputConfig
  { _inputConfig_initialValue :: a
  , _inputConfig_setValue     :: Event t a
  , _inputConfig_label        :: Dynamic t Text
  , _inputConfig_status       :: Dynamic t InputStatus
  , _inputConfig_attributes   :: Map AttributeName Text
  }

data InputEl t a = InputEl
  { _inputEl_value :: Dynamic t a
  , _inputEl_hasFocus :: Dynamic t Bool
  }

instance (Default a, Reflex t) => Default (InputConfig t a) where
  def = InputConfig { _inputConfig_initialValue = def
                    , _inputConfig_setValue     = never
                    , _inputConfig_label        = constDyn ""
                    , _inputConfig_status       = def
                    , _inputConfig_attributes   = def
                    }

instance Reflex t => Functor (InputConfig t) where
  fmap f cfg = cfg
    { _inputConfig_initialValue = f $ _inputConfig_initialValue cfg
    , _inputConfig_setValue     = f <$> _inputConfig_setValue cfg
    }

data NumberInputConfig a = NumberInputConfig
  { _numberInputConfig_maxValue :: Maybe a
  , _numberInputConfig_minValue :: Maybe a
  , _numberInputConfig_precision :: Maybe Int
  }

instance Default (NumberInputConfig a) where
  def = NumberInputConfig Nothing Nothing Nothing

inputStyle :: Css
inputStyle = do
  formStyle
  inputElementStyle
  selectElementStyle
  textAreaElementStyle
  checkboxStyle
  toggleStyle
  ".inlineabs" ? do
    Clay.display inlineBlock
    verticalAlign vAlignTop

toggleStyle :: Css
toggleStyle = do
  input # ("type" @= "checkbox") # ".toggle" ? do
    marginLeft (px 5)
    marginRight (px 22)

    before Clay.& do
      left (px (-5))
      width (px 34)
      height (px 18)
      backgroundColor nord3'
      borderWidth (px 0)
      borderRadius (px 9) (px 9) (px 9) (px 9)
      transitionDuration 0.1
      transitionTimingFunction easeIn

      checked Clay.& do
        backgroundColor nord10'
        transitionDuration 0.1
        transitionTimingFunction easeIn

    after Clay.& do
      absoluteBlock
      width (px 14)
      height (px 14)
      left (px (-3))
      top (px 2)
      borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
      background white0'
      transitionDuration 0.1
      transitionTimingFunction easeIn

      checked Clay.& do
        left (px 13)
        borderWidth (px 0)
        transitionDuration 0.1
        transitionTimingFunction easeIn

borderRadiusAll :: Size a -> Css
borderRadiusAll x = borderRadius x x x x

checkboxStyle :: Css
checkboxStyle = do
  ".checkbox-label" ? do
    cursor pointer
    fontWeight normal

  input # ("type" @= "checkbox") ? do
    position relative
    cursor pointer

    before Clay.& do
      absoluteBlock
      width (px 16)
      height (px 16)
      borderRadiusAll (px 3)
      border solid (px 1) grey0'
      backgroundColor white0'

    checked Clay.& do
      before Clay.& do
        borderColor nord10'
        backgroundColor nord10'

      after Clay.& do
        width (px 5)
        height (px 10)
        borderStyle solid
        borderColor white0'
        borderWidth4 (px 0) (px 2) (px 2) (px 0)
        transform (rotate (deg 45))
        top (px 2)
        left (px 6)

absoluteBlock :: Css
absoluteBlock = do
  content (stringContent "")
  Clay.display block
  position absolute


inputElementStyle :: Css
inputElementStyle = do
  (input <> Clay.select <> textarea) ? do
    background transparent
    borderWidth 0
    borderBottomWidth 1
    padding (px 4) (px 4) (px 4) (px 4)
    borderColor grey0'
    outlineWidth 0

    focus Clay.& do
      borderBottomWidth 2
      borderColor nord10'
      marginBottom (px (-1))

    "::placeholder" Clay.& do
      fontColor grey0'

    disabled Clay.& do
      fontColor grey0'
      cursor notAllowed

    ".has-error" Clay.& do
      borderColor nord11'

    ".has-success" Clay.& do
      borderColor green1'

selectElementStyle :: Css
selectElementStyle = do
  Clay.select ? do
    cursor pointer
    width (pc 100)
    maxWidth (px 185)
    "appearance" -: "none"
    "-webkit-appearance" -: "none"
    "-moz-appearance" -: "none"

  Clay.select |+ ".select-icon" ? do
    Clay.display inlineBlock
    transform (translate (px (-24)) (px (-12)))
    pointerEvents none

textAreaElementStyle :: Css
textAreaElementStyle = textarea ? do
  background white
  borderWidth (px 1)
  borderRadiusAll (px 3)
  minWidth (px 300)
  minHeight (px 100)

  disabled Clay.& do
    background white0'
    cursor notAllowed

formStyle :: Css
formStyle = do
  label ? do
    fontWeight bold
    lineHeight (px 23)

  form ? do
    Clay.display grid
    key "grid-column-gap"       (px 50)
    key "grid-row-gap"          (px 5)
    key "grid-template-columns" (pct 25, pct 75)

    ".helptext" ? do
      fontSize (px 12)

    ".helptext" # ".has-error" ? do
      fontColor nord11'

    ".helptext" # ".has-success" ? do
      fontColor green1'

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

  pure $ leftmost [modAttrEv, postBuildAttrEv]

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
  cfg = def { _iconConfig_size = 24 }
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
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => InputConfig t Text
  -> m (InputEl t Text)
textInput cfg = labeled cfg textInput'

textInput'
  :: (PostBuild t m, DomBuilder t m)
  => Text
  -> InputConfig t Text
  -> m (InputEl t Text)
textInput' idStr cfg = do
  modAttrEv <- statusModAttrEv' cfg

  elAttr "div" ("style" =: "display:inline-block") $ do
    n <-
      inputElement
      $  def
      &  inputElementConfig_initialValue
      .~ _inputConfig_initialValue cfg
      &  inputElementConfig_setValue
      .~ _inputConfig_setValue cfg
      &  inputElementConfig_elementConfig
      .  elementConfig_initialAttributes
      .~ _inputConfig_attributes cfg
      <> "id"
      =: idStr
      &  inputElementConfig_elementConfig
      .  elementConfig_modifyAttributes
      .~ modAttrEv

    statusMessageIcon (_inputConfig_status cfg)

    statusMessageElement (_inputConfig_status cfg)

    pure $ InputEl { _inputEl_value    = _inputElement_value n
                   , _inputEl_hasFocus = _inputElement_hasFocus n
                   }

numberInput
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , RealFloat a
     , MonadIO m
     )
  => NumberInputConfig a
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
  => NumberInputConfig a
  -> Text
  -> InputConfig t a
  -> m (Dynamic t (Maybe a))
numberInput' nc idStr cfg = do
  let initAttrs =
        "type"
          =: "number"
          <> "style"
          =: "text-align:right"
          <> "onClick"
          =: "this.select()"
          <> Map.fromList
               (catMaybes
                 [ ("max", ) . prnt <$> _numberInputConfig_maxValue nc
                 , ("min", ) . prnt <$> _numberInputConfig_minValue nc
                 , ("step", ) . mkStep <$> _numberInputConfig_precision nc
                 ]
               )
      styleChange (Just x) _ _
        | maybe False (x >) (_numberInputConfig_maxValue nc)
        = InputError $ "Exceeds the maximum " <> prnt
          (fromJust (_numberInputConfig_maxValue nc))
        | maybe False (x <) (_numberInputConfig_minValue nc)
        = InputError $ "Is less than the minimum " <> prnt
          (fromJust (_numberInputConfig_minValue nc))
        | otherwise
        = def
      styleChange Nothing hasFocus t
        | Text.null t || hasFocus = def
        | -- Don't update the value when still typing
          otherwise               = InputError "Not a valid number"

  rec
    n <- textInput'
      idStr
      cfg
        { _inputConfig_initialValue = prnt $ _inputConfig_initialValue cfg
        , _inputConfig_setValue     = leftmost
          [prnt <$> _inputConfig_setValue cfg, zeroIfEmptyEv]
        , _inputConfig_status       = zipDynWith max
                                                 (_inputConfig_status cfg)
                                                 statusDyn
        , _inputConfig_attributes   = _inputConfig_attributes cfg <> initAttrs
        }
    let
      result        = readMaybe . unpack <$> _inputEl_value n
      zeroIfEmptyEv = attachPromptlyDynWithMaybe
        emptyNoFocus
        ((,) <$> _inputEl_value n <*> result)
        (updated (_inputEl_hasFocus n))
      modAttrEv = updated
        (styleChange <$> result <*> _inputEl_hasFocus n <*> _inputEl_value n)
    statusDyn <- holdDyn def modAttrEv
  return result
 where
  mkStep :: Int -> Text
  mkStep x = prnt (10 ^^ (-x))
  prnt x = pack $ showFFloatAlt (_numberInputConfig_precision nc) x ""
  emptyNoFocus (x, r) f | Text.null x && not f = Just (prnt 0)
                        | not f                = prnt <$> r
                        | otherwise            = Nothing

toggleInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => InputConfig t Bool
  -> m (Dynamic t Bool)
toggleInput cfg = do
  let initAttrs = "class" =: "toggle"

  checkboxInput cfg
    { _inputConfig_attributes = _inputConfig_attributes cfg <> initAttrs
    }

checkboxInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => InputConfig t Bool
  -> m (Dynamic t Bool)
checkboxInput cfg = elClass "div" "inlineabs" $ do
  modAttrEv <- statusModAttrEv' cfg
  idStr     <- randomId

  result    <- el "div" $ do
    n <-
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
      .~ modAttrEv

    elAttr "label" ("for" =: idStr <> "class" =: "checkbox-label")
      $ dynText (_inputConfig_label cfg)

    statusMessageIcon (_inputConfig_status cfg)

    pure (_inputElement_checked n)

  statusMessageElement (_inputConfig_status cfg)
  pure result

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
  -> m (InputEl t (Maybe a))
selectInput' idStr cfg = do
  modAttrEv <- statusModAttrEv' cfg

  elAttr "div" ("style" =: "display:inline-block") $ do
    n <- selectElement
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

    elClass "div" "select-icon" arrowElement

    statusMessageIcon (_inputConfig_status cfg)

    statusMessageElement (_inputConfig_status cfg)

    pure $ InputEl { _inputEl_value = parseEnum <$> _selectElement_value (fst n)
                   , _inputEl_hasFocus = _selectElement_hasFocus (fst n)
                   }
 where
  arrowElement =
    icon def { _iconConfig_direction = constDyn DirDown } angleIcon

  allPossible :: (Enum a, Bounded a) => Maybe a -> [a]
  allPossible _ = [minBound .. maxBound]

  showNum :: Enum a => Maybe a -> Text
  showNum (Just x) = pack . show . fromEnum $ x
  showNum Nothing  = mempty

  parseEnum :: Enum a => Text -> Maybe a
  parseEnum = fmap toEnum . readMaybe . unpack

  mkOption x = elAttr "option" ("value" =: showNum (Just x)) (text (toLabel x))

textAreaInput
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => InputConfig t Text
  -> m (Dynamic t Text)
textAreaInput cfg = _inputEl_value <$> labeled cfg textAreaInput'

textAreaInput'
  :: (PostBuild t m, DomBuilder t m)
  => Text
  -> InputConfig t Text
  -> m (InputEl t Text)
textAreaInput' idStr cfg = do
  modAttrEv <- statusModAttrEv' cfg

  elAttr "div" ("style" =: "display:inline-block") $ do
    n <- textAreaElement
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
      .~ modAttrEv
      )

    statusMessageIcon (_inputConfig_status cfg)

    statusMessageElement (_inputConfig_status cfg)

    pure $ InputEl { _inputEl_value    = _textAreaElement_value n
                   , _inputEl_hasFocus = _textAreaElement_hasFocus n
                   }
