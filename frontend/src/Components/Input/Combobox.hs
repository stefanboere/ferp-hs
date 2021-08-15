{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Components.Input.Combobox
  ( comboboxStyle
  , ComboboxValue(..)
  , comboboxInput
  , comboboxInput'
  , altSelectInput
  , altSelectInput'
  ) where

import           Prelude                 hiding ( (**)
                                                , rem
                                                )

import           Clay                    hiding ( icon )
import           Control.Applicative            ( Const(..) )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Default
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Reflex.Dom              hiding ( (&)
                                                , display
                                                )
import           Text.Fuzzy

import           Components.Input.Basic
import           Components.Progress
import           Nordtheme

comboboxStyle :: Css
comboboxStyle = singleComboboxStyle <> multiComboboxStyle

singleComboboxStyle :: Css
singleComboboxStyle = ".combobox-menu" ? do
  display block
  important $ top (rem (7 / 4))
  width (pct 100)

  (option <> i) ? do
    Clay.display flex
    justifyContent spaceBetween
    alignItems center
    paddingLeft (rem 1)
    height (rem (3 / 2))
    textTransform none
    backgroundColor inherit
    color nord3'
    "fill" -: showColor nord3'

  i ? paddingBottom (rem (1 / 4))

  option ? do
    cursor pointer

    ".active" Clay.& do
      background nord4'
      hover Clay.& backgroundColor nord4'

    hover Clay.& background nord6'

multiComboboxStyle :: Css
multiComboboxStyle = ".multi-combobox" ? do
  borderStyle solid
  paddingTop nil
  paddingBottom nil
  paddingRight nil

  ".combobox" ? do
    borderBottomWidth nil

data ComboboxValue k = ComboboxValue
  { _cb_selection :: k
  , _cb_text      :: Text
  }
  deriving (Show, Eq)

instance Default k => Default (ComboboxValue k) where
  def = ComboboxValue def mempty

comboboxInput
  :: forall t m k
   . (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, Ord k)
  => (Dynamic t k -> Dynamic t Text -> m ())
  -> Dynamic t (Map k Text)
  -> InputConfig t m (ComboboxValue (Maybe k))
  -> m (DomInputEl t m (ComboboxValue (Maybe k)))
comboboxInput = comboboxInput' (constDyn False)

comboboxInput'
  :: forall t m k
   . (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, Ord k)
  => Dynamic t Bool
  -> (Dynamic t k -> Dynamic t Text -> m ())
  -> Dynamic t (Map k Text)
  -> InputConfig t m (ComboboxValue (Maybe k))
  -> m (DomInputEl t m (ComboboxValue (Maybe k)))
comboboxInput' loadingDyn showOpt allOptions cfg = do
  rec
    (searchStrInput, selectEv) <- textInputWithIco'
      (after' dynSelection options hasFocusDyn setOpenEv)
      (_cb_text <$> cfg)
        { _inputConfig_attributes = _inputConfig_attributes cfg
                                    <> maybe
                                         mempty
                                         ( Map.singleton "list"
                                         . (<> "-datalist")
                                         )
                                         (_inputConfig_id cfg)
                                    <> "class"
                                    =: "combobox"
        , _inputConfig_setValue   = leftmost
          [_inputConfig_setValue (_cb_text <$> cfg), setTextOnLostFocusEv]
        , _inputConfig_status     = _inputConfig_status cfg <> dynStatus
        , _inputConfig_eventSpec  = addEventSpecFlags
                                      (Nothing :: Maybe (DomBuilderSpace m))
                                      Keydown
                                      preventArrowDef
        }
    let hasFocusDyn = _inputEl_hasFocus searchStrInput

-- Filter possible keys with fuzzyfind
    let options =
          filterOptions <$> _inputEl_value searchStrInput <*> allOptions
    let keyIndices =
          Map.fromList
            .   fmap (\(ind, (k, t)) -> (k, (ind, t)))
            .   Map.toList
            <$> options

-- Clear the selection if the text is null
    let clearEv  = ffilter Text.null $ updated (_inputEl_value searchStrInput)

-- Tab completion
    let inputEls = _inputEl_elements searchStrInput
    let arrowUpDownEv = ffilter (`elem` 9 : arrowKeys)
          $ switchDyn (leftmost . fmap (domEvent Keydown) <$> inputEls)

-- Whenever the current selection is not in the list any more, clear it
    let selectNextEv = attachPromptlyDynWith selectNext
                                             ((,) <$> options <*> keyIndices)
                                             arrowUpDownEv
    let setOpenEv = False <$ ffilter (== 13) arrowUpDownEv

-- Update the text on lose focus if a value has been selected
    let
      lostFocusEv =
        gate
            (current (Prelude.not . Text.null <$> _inputEl_value searchStrInput)
            )
          $ ffilter Prelude.not (updated hasFocusDyn)
    let setTextOnLostFocusEv = attachPromptlyDynWith
          selectionText
          keyIndices
          (tagPromptlyDyn dynSelection
                          (leftmost [() <$ lostFocusEv, () <$ arrowUpDownEv])
          )

    dynSelection <-
      foldDyn ($) (_cb_selection $ _inputConfig_initialValue cfg) $ leftmost
        [const . Just <$> selectEv, const Nothing <$ clearEv, selectNextEv]

-- Show an error if selection is null and the value is not null, and show it on lose focus
    let dynStatus =
          mkStatus
            <$> _inputEl_value searchStrInput
            <*> dynSelection
            <*> hasFocusDyn

  let comboVal =
        ComboboxValue <$> dynSelection <*> _inputEl_value searchStrInput

  pure $ InputEl comboVal hasFocusDyn inputEls
 where
  mkStatus :: Text -> Maybe k -> Bool -> InputStatus
  mkStatus x Nothing False
    | Prelude.not (Text.null x)
    = InputError $ "The option " <> x <> " could not be found."
    | otherwise
    = InputNeutral Nothing
  mkStatus _ _ _ = InputNeutral Nothing

  selectionText :: Map k (a, Text) -> Maybe k -> Text
  selectionText _ Nothing  = ""
  selectionText m (Just k) = maybe "" snd $ m Map.!? k

  autofill opts = fst . snd <$> Map.lookupMin opts

  selectNext
    :: (Map Integer (k, Text), Map k (Integer, Text))
    -> Word
    -> (Maybe k -> Maybe k)
  selectNext _              13  x        = x
  selectNext (opts, _     ) _   Nothing  = autofill opts
  selectNext (opts, keyInd) key (Just k) = fromMaybe (Just k) $ do
    (ind, _) <- keyInd Map.!? k
    let offs = case key of
          38 -> -1
          40 -> 1
          _  -> 0
    r <- opts Map.!? (ind + offs)
    pure $ Just (fst r)

  filterOptions :: Text -> Map k Text -> Map Integer (k, Text)
  filterOptions pat opts
    | pat `elem` Map.elems opts
    = Map.fromList $ zip [0, 1 ..] (Map.toList opts)
    | otherwise
    = let r = Text.Fuzzy.filter pat (Map.toList opts) "" "" snd False
      in  Map.fromList $ zip [0, 1 ..] (Prelude.map original r)

  after' dynSelection options hasFocusDyn setOpenEv = do
    selectIcon

    rec (e, dynOptEv) <- elDynAttr' "datalist" (mkDatalistAttr <$> openDyn) $ do
          elDynClass
              "i"
              (   (\opts loading ->
                    mkVisible (Map.null opts && Prelude.not loading)
                  )
              <$> options
              <*> loadingDyn
              )
            $ text "No results found"
          r <- listViewWithKey options (mkOption dynSelection)
          elDynClass "i" (mkVisible <$> loadingDyn)
            $ spinner Inline "Searching for matches"
          pure r

  -- This keeps the dropdown open while the user is clicking an item, even though the input has lost focus
        mousePressed <- holdDyn False $ leftmost
          [True <$ domEvent Mousedown e, False <$ domEvent Mouseup e]

        openDyn <- holdDyn False $ leftmost
          [ gate (Prelude.not <$> current mousePressed) (updated hasFocusDyn)
          , False <$ dynOptEv
          , setOpenEv
          ]

    let
      setOpenAttrEv =
        (\isOpen ->
            Map.singleton "class" (if isOpen then Just "open" else Nothing)
          )
          <$> updated openDyn

    let selectedKeyEv = snd . Map.findMin <$> dynOptEv

    pure (fst <$> selectedKeyEv, (setOpenAttrEv, snd <$> selectedKeyEv))

  mkVisible True  = ""
  mkVisible False = "hidden"

  mkOption dynSelection _ dynOpt = do
    let (dynK, dynV) = splitDynPure dynOpt
    (e, _) <- elDynAttr'
      "option"
      (  (("value" =:) . snd <$> dynOpt)
      <> (mkCurrentCls <$> dynK <*> dynSelection)
      )
      (showOpt dynK dynV)
    let clickEv = domEvent Click e
    pure $ tagPromptlyDyn dynOpt clickEv

  mkCurrentCls _ Nothing = Map.empty
  mkCurrentCls k (Just kSel) | k == kSel = Map.singleton "class" "active"
                             | otherwise = Map.empty

  mkDatalistAttr isOpen =
    Map.singleton "class" ("combobox-menu" <> if isOpen then " open" else "")
      <> maybe mempty (Map.singleton "id") (_inputConfig_id cfg)

  arrowKeys = [13, 38, 40]
  preventArrowDef Nothing = preventDefault
  preventArrowDef (Just (EventResult key)) =
    if key `elem` arrowKeys then preventDefault else mempty

-- | A select input without using the select html element
altSelectInput'
  :: ( PostBuild t m
     , DomBuilder t m
     , Enum a
     , Bounded a
     , Ord a
     , MonadHold t m
     , MonadFix m
     )
  => (a -> Text)
  -> EnumInputConfig t m (Maybe a)
  -> m (DomInputEl t m (Maybe a))
altSelectInput' toLabel cfg = fmap _cb_selection <$> comboboxInput
  printElem
  (constDyn $ allPossibleMap toLabel)
  (fmap toCb (cfg { _inputConfig_extra = Const () }))
 where
  printElem k _ = dyn_ (getOpElem (_inputConfig_extra cfg) <$> k)
  toCb k = ComboboxValue { _cb_selection = k, _cb_text = maybe "" toLabel k }

-- | A select input without using the select html element, using show instance to produce search text
altSelectInput
  :: ( PostBuild t m
     , DomBuilder t m
     , Enum a
     , Bounded a
     , Ord a
     , Show a
     , MonadHold t m
     , MonadFix m
     )
  => EnumInputConfig t m (Maybe a)
  -> m (DomInputEl t m (Maybe a))
altSelectInput = altSelectInput' (Text.pack . show)
