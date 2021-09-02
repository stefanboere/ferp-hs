{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Components.Input.Combobox
  ( comboboxStyle
  , ComboboxValue(..)
  , comboboxInput
  , comboboxInputKS'
  , multiComboboxInput
  , altSelectInput
  , altSelectInput'
  , OptionsRequest(..)
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
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Reflex.Dom              hiding ( (&)
                                                , display
                                                )
import           Text.Fuzzy

import           Components.Class
import           Components.Input.Basic
import           Components.Progress
import           Components.Tag
import           Nordtheme

comboboxStyle :: Css
comboboxStyle = do
  ".combobox-menu" ? do
    display block
    important $ top (rem (7 / 4))
    width (pct 100)

    (option <> ".option" <> i) ? do
      Clay.display flex
      alignItems center
      paddingLeft (rem 1)
      paddingRight (rem 1)
      height (rem (3 / 2))
      textTransform none
      backgroundColor inherit
      color nord3'
      "fill" -: showColor nord3'

    ".icon" ? marginRight (rem (1 / 4))

    i ? paddingBottom (rem (1 / 4))

    (option <> ".option") ? do
      cursor pointer

      ".active" Clay.& do
        background nord4'
        hover Clay.& backgroundColor nord4'

      hover Clay.& background nord6'

  ".combobox-wrapper" ? do
    borderStyle solid
    paddingTop nil
    paddingBottom nil
    paddingRight nil

    ".disabled" Clay.& ".tag" ? do
      cursor notAllowed
      hover Clay.& backgroundColor inherit

    ".tag" ? do
      marginTop (rem (1 / 8))
      marginBottom (rem (1 / 8))
      paddingRight (rem (1 / 4))

    ":focus-within" Clay.& do
      borderBottomWidth 2
      borderColor nord10'
      marginBottom (px (-1))

    ".combobox" ? do
      borderBottomWidth nil


data ComboboxValue k = ComboboxValue
  { _cb_selection :: k
  , _cb_text      :: Text
  }
  deriving (Show, Eq)

instance Default k => Default (ComboboxValue k) where
  def = ComboboxValue def mempty

instance Functor ComboboxValue where
  fmap f x = x { _cb_selection = f (_cb_selection x) }

comboboxInput
  :: forall t m k
   . (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, Ord k)
  => (Dynamic t k -> Dynamic t Text -> m ())
  -> Map k Text
  -> InputConfig t m (ComboboxValue (Maybe k))
  -> m (DomInputEl t m (ComboboxValue (Maybe k)))
comboboxInput showOpt allOptions cfg = do
  (x, _, _) <- comboboxInputKS' (pure ())
                                (pure ())
                                showOpt
                                (universeList . Map.toList $ allOptions)
                                (filterPureFuzzy allOptions)
                                cfg
  pure x

universeList :: [k] -> MapSubset Int k
universeList = universe . Map.fromList . zip [0 ..]

filterPureFuzzy
  :: (Reflex t, Applicative m)
  => Map k Text
  -> Event t (OptionsRequest k)
  -> m (Event t (Either Text (MapSubset Int (k, Text))))
filterPureFuzzy allOptions reqEv = pure (Right . mkResult <$> reqEv)
 where
  mkResult (SearchText searchText) =
    universeList $ filterOptions searchText allOptions
  mkResult _ = universeList $ filterOptions "" allOptions

filterOptions :: Text -> Map k Text -> [(k, Text)]
filterOptions pat opts
  | pat `elem` Map.elems opts
  = Map.toList opts
  | otherwise
  = let r = Text.Fuzzy.filter pat (Map.toList opts) "" "" snd False
    in  Prelude.map original r

data OptionsRequest k
  = NearSelection (Maybe k)
  | SearchText Text
  deriving (Eq, Show)

comboboxInputKS'
  :: forall t m k b aftr
   . (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, Ord k)
  => m b
  -> m aftr
  -> (Dynamic t k -> Dynamic t Text -> m ())
  -> MapSubset Int (k, Text)
  -> (  Event t (OptionsRequest k)
     -> m (Event t (Either Text (MapSubset Int (k, Text))))
     )
  -> InputConfig t m (ComboboxValue (Maybe k))
  -> m (DomInputEl t m (ComboboxValue (Maybe k)), b, aftr)
comboboxInputKS' before' aftr' showOpt initOpts mkSetOptsEv cfg = do
  rec
    (searchStrInput, b', (selectEv, mousePressed, a', dynOptions)) <-
      textInputWithIco''
        "combobox-wrapper select"
        before'
        (after' sdemux dynCount updateRows hasFocusDyn loadingDyn setOpenEv)
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
          , _inputConfig_status     = dynError
                                      <> _inputConfig_status cfg
                                      <> dynStatus
          , _inputConfig_eventSpec  = addEventSpecFlags
                                        (Nothing :: Maybe (DomBuilderSpace m))
                                        Keydown
                                        preventArrowDef
          }
    hasFocusDyn <-
      holdUniqDyn $ (||) <$> _inputEl_hasFocus searchStrInput <*> mousePressed

    let reqEv = leftmost
          [ NearSelection . fmap fst <$> tag
            (current dynSelection)
            (ffilter Prelude.id (updated hasFocusDyn))
          , SearchText <$> gate (current hasFocusDyn)
                                (updated (_inputEl_value searchStrInput))
          ]
    setOptsEvResult <- mkSetOptsEv reqEv

    let (setOptsEvErr, setOptsEv) = fanEither setOptsEvResult

    loadingDyn <- holdDyn False
      $ leftmost [False <$ setOptsEvResult, True <$ reqEv]

    dynError <- holdDyn def
      $ leftmost [InputError <$> setOptsEvErr, def <$ setOptsEv]

    let updateRows =
          attachWith addDeletes (Map.keysSet <$> current dynOptions) setOptsEv
    dynCount <- holdDyn (_ms_totalCount initOpts) (_ms_totalCount <$> setOptsEv)

-- Clear the selection if the text is null
    let clearEv = ffilter Text.null $ updated (_inputEl_value searchStrInput)
    let clearNoOptionEv = ffilter (== Just 0) $ updated dynCount
    let autofillEv      = ffilter (== Just 1) $ updated dynCount

-- Tab completion
    let inputEls        = _inputEl_elements searchStrInput
    let arrowUpDownEv = ffilter (`elem` 9 : arrowKeys)
          $ switchDyn (leftmost . fmap (domEvent Keydown) <$> inputEls)

-- Whenever the current selection is not in the list any more, clear it
    let setOpenEv = False <$ ffilter (== 13) arrowUpDownEv

-- Update the text on lose focus if a value has been selected
    let
      lostFocusEv =
        gate
            (current (Prelude.not . Text.null <$> _inputEl_value searchStrInput)
            )
          $ ffilter Prelude.not (updated hasFocusDyn)
    let setTextOnLostFocusEv = fmapMaybe (fmap snd) $ tagPromptlyDyn
          dynSelection
          (leftmost [() <$ lostFocusEv, () <$ arrowUpDownEv])

    selectedIndex <- foldDyn ($) (findIndex initialKey initOpts) $ leftmost
      [ const (Just 0) <$ autofillEv
      , attachWith selectNext (current dynCount) arrowUpDownEv
      , const Nothing <$ clearEv
      , const Nothing <$ clearNoOptionEv
      , const . Just <$> selectEv
-- TODO setOptsEv, _inputConfig_setValue event
      ]

    let dynSelection =
          (\o' i' -> (o' Map.!?) =<< i') <$> dynOptions <*> selectedIndex
        sdemux = demux selectedIndex

-- Show an error if selection is null and the value is not null, and show it on lose focus
    let dynStatus =
          mkStatus
            <$> _inputEl_value searchStrInput
            <*> (fmap fst <$> dynSelection)
            <*> hasFocusDyn

  let comboVal =
        ComboboxValue
          <$> (fmap fst <$> dynSelection)
          <*> _inputEl_value searchStrInput

  pure (InputEl comboVal hasFocusDyn inputEls, b', a')
 where
  mkStatus :: Text -> Maybe k -> Bool -> InputStatus
  mkStatus x Nothing False
    | Prelude.not (Text.null x)
    = InputError $ "The option " <> x <> " could not be found."
    | otherwise
    = InputNeutral Nothing
  mkStatus _ _ _ = InputNeutral Nothing

  initialKey = _cb_selection $ _inputConfig_initialValue cfg
  findIndex (Just k) (MapSubset d _) =
    fmap fst . Map.lookupMin $ Map.filter ((== k) . fst) d
  findIndex Nothing _ = Nothing

  selectNext :: Maybe Integer -> Word -> Maybe Int -> Maybe Int
  selectNext _ 13 x       = x
  selectNext _ _  Nothing = Just 0
  selectNext m key (Just k) =
    let offs = case key of
          38 -> -1
          40 -> 1
          _  -> 0
        setMax = maybe Prelude.id (Prelude.min . fromIntegral . subtract 1) m
    in  Just (setMax (Prelude.max 0 (k + offs)))

  after' sdemux dynCount updateRows hasFocusDyn loadingDyn setOpenEv = do
    selectIcon
    a' <- aftr'

    rec
      (e, dynOptEv') <- elDynAttr' "div" (mkDatalistAttr <$> openDyn) $ do
        elDynClass
            "i"
            (   (\c loading -> mkVisible (c == Just 0 && Prelude.not loading))
            <$> dynCount
            <*> loadingDyn
            )
          $ text "No results found"
        r <- listWithKeyShallowDiff (_ms_data initOpts)
                                    (_ms_data <$> updateRows)
                                    (mkOption sdemux)
        elDynClass "i" (mkVisible <$> loadingDyn)
          $ spinner Inline "Searching for matches"
        pure r

      let selectedIndexEv = fst . Map.findMin <$> switchDyn
            (mergeMap . fmap fst <$> dynOptEv')

-- This keeps the dropdown open while the user is clicking an item, even though the input has lost focus
      mousePressed <- holdDyn False
        $ leftmost [True <$ domEvent Mousedown e, False <$ domEvent Mouseup e]

      openDyn <- holdDyn False $ leftmost
        [ gate (Prelude.not <$> current mousePressed) (updated hasFocusDyn)
        , False <$ selectedIndexEv
        , setOpenEv
        ]

    let
      setOpenAttrEv =
        (\isOpen ->
            Map.singleton "class" (if isOpen then Just "open" else Nothing)
          )
          <$> updated openDyn

    let dynOpt = joinDynThroughMap (fmap snd <$> dynOptEv')

    let selectedKeyEv =
          attachPromptlyDynWithMaybe (Map.!?) dynOpt selectedIndexEv

    pure
      ( (selectedIndexEv, mousePressed, a', dynOpt)
      , (setOpenAttrEv, snd <$> selectedKeyEv)
      )

  mkVisible True  = ""
  mkVisible False = "hidden"

  mkOption sdemux ind initOpt setOptEv = do
    dynOpt <- holdDyn initOpt setOptEv
    let (dynK, dynV) = splitDynPure dynOpt
    let dynSelection = demuxed sdemux (Just ind)

    (e, _) <- elDynAttr'
      "div"
      ((("value" =:) <$> dynV) <> (mkCurrentCls <$> dynSelection))
      (showOpt dynK dynV)
    let clickEv = domEvent Click e
    pure (i <$ clickEv, dynOpt)

  mkCurrentCls False = Map.singleton "class" "option"
  mkCurrentCls True  = Map.singleton "class" "option active"

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
  (allPossibleMap toLabel)
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

multiComboboxInput
  :: forall t m k
   . (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, Ord k)
  => (Dynamic t k -> Dynamic t Text -> m ())
  -> Map k Text
  -> InputConfig t m (ComboboxValue (Set k))
  -> m (DomInputEl t m (ComboboxValue (Set k)))
multiComboboxInput showOpt allOptions cfg = do
  rec
    (r, dEv, _) <- comboboxInputKS'
      (before' dynValue)
      (pure ())
      showOpt
      (universeList $ Map.toList allOptions)
      (filterPureFuzzy allOptions)
      (cfg { _inputConfig_initialValue = def
           , _inputConfig_setValue     = leftmost [def <$ addOnChange]
           , _inputConfig_extra        = Const ()
           }
      )

    let searchText   = _cb_text <$> _inputEl_value r

    let selectionDyn = _cb_selection <$> _inputEl_value r

    let addOnChange = fmapMaybe Prelude.id $ gate
          (current (Prelude.not <$> _inputEl_hasFocus r))
          (updated selectionDyn)

    dynPop <- hold Nothing (Just <$> addOnChange)
    let addOnChange' =
          attachWithMaybe onNothingPop dynPop (updated selectionDyn)

    let enabledDyn = current ((/= InputDisabled) <$> _inputConfig_status cfg)

    dynValue <-
      foldDyn ($) (_cb_selection $ _inputConfig_initialValue cfg) $ leftmost
        [ gate enabledDyn $ Set.insert <$> addOnChange'
        , gate enabledDyn ((\m xs -> Set.difference xs (Map.keysSet m)) <$> dEv)
        , const . _cb_selection <$> _inputConfig_setValue cfg
        ]
  pure $ r { _inputEl_value = ComboboxValue <$> dynValue <*> searchText }
 where
  onNothingPop x Nothing = x
  onNothingPop _ _       = Nothing

  before' dynValues =
    listViewWithKey (Map.restrictKeys allOptions <$> dynValues) $ \_ v -> do
      tagEl def { _tagConfig_action = Just TagDismiss } v

