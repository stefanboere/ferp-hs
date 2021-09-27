{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Input.Combobox
  ( comboboxStyle
  , ComboboxValue(..)
  , comboboxInput
  , comboboxInputKS
  , multiComboboxInput
  , altSelectInput
  , altSelectInput'
  , OptionsRequest(..)
  )
where

import           Prelude                 hiding ( (**)
                                                , rem
                                                )

import           Clay                    hiding ( icon )
import           Control.Applicative            ( Const(..) )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Default
import           Data.Fixed                     ( divMod' )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
import           GHCJS.DOM.Element              ( setScrollTop )
import           GHCJS.DOM.Types                ( MonadJSM )
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
      marginTop (rem (1 / 4))
      marginBottom (rem (1 / 4))
      paddingRight (rem (1 / 4))

    ":focus-within" Clay.& do
      borderBottomWidth 2
      borderColor nord10'
      marginBottom (px (-1))

    ".combobox" ? do
      borderBottomWidth nil
      focus Clay.& marginBottom nil


data ComboboxValue k = ComboboxValue
  { _cb_selection :: !k
  , _cb_text      :: !Text
  }
  deriving (Show, Eq)

instance Default k => Default (ComboboxValue k) where
  def = ComboboxValue def mempty

instance Functor ComboboxValue where
  fmap f x = x { _cb_selection = f (_cb_selection x) }

comboboxInput
  :: forall t m k js
   . ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , Ord k
     , Prerender js t m
     )
  => (Dynamic t k -> Dynamic t Text -> Client m ())
  -> Map k Text
  -> InputConfig t m (ComboboxValue (Maybe k))
  -> m (DomInputEl t m (ComboboxValue (Maybe k)))
comboboxInput showOpt allOptions cfg = do
  (x, _) <- comboboxInputKS (pure ())
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
  -> m
       ( Event t (Either Text (MapSubset Int (k, Text)))
       , Dynamic t Bool
       )
filterPureFuzzy allOptions reqEv = pure
  (Right . mkResult <$> reqEv, constDyn False)
 where
  mkResult (SearchText searchText _) =
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
  = NearSelection (Maybe k) Int -- ^ Selected key and limit
  | SearchText Text (Int, Int) -- ^ Search query, and limit and offset
  deriving (Eq, Show)

comboboxInputWrapper
  :: (DomBuilder t m, PostBuild t m, MonadFix m)
  => m b
  -> m (a, Dynamic t InputStatus)
  -> m (a, b)
comboboxInputWrapper before' cbInput = do
  elClass "div" "input" $ do
    rec ((n, dynStatus), bf) <- elClass "div" "flex-row" $ do
          (n', bf') <-
            elDynClass
                "div"
                (   joinCls ["flex-row combobox-wrapper select"]
                .   mkCls
                <$> dynStatus
                )
              $ do
                  bf'' <- before'
                  n''  <- cbInput
                  pure (n'', bf'')

          statusMessageIcon dynStatus
          pure (n', bf')

        statusMessageElement dynStatus

    pure (n, bf)

 where
  joinCls xs x =
    Text.unwords $ Prelude.filter (Prelude.not . Text.null) (x : xs)
  mkCls InputDisabled = "disabled"
  mkCls x             = colorCls x

comboboxInputKS
  :: forall t m k b js
   . ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , Ord k
     , Prerender js t m
     )
  => m b
  -> (Dynamic t k -> Dynamic t Text -> Client m ())
  -> MapSubset Int (k, Text)
  -> (  Event t (OptionsRequest k)
     -> m
          ( Event t (Either Text (MapSubset Int (k, Text)))
          , Dynamic t Bool
          )
     )
  -> InputConfig t m (ComboboxValue (Maybe k))
  -> m (DomInputEl t m (ComboboxValue (Maybe k)), b)
comboboxInputKS before' showOpt initOpts mkSetOptsEv cfg = comboboxInputWrapper
  before'
  (comboboxInputRawInput (_inputConfig_setValue cfg)
                         showOpt
                         initOpts
                         mkSetOptsEv
                         cfg
  )

comboboxInputKS'
  :: forall t m k b js
   . ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , Ord k
     , Prerender js t m
     )
  => m b
  -> Event t (ComboboxValue (Maybe k))
  -> (Dynamic t k -> Dynamic t Text -> Client m ())
  -> MapSubset Int (k, Text)
  -> (  Event t (OptionsRequest k)
     -> m
          ( Event t (Either Text (MapSubset Int (k, Text)))
          , Dynamic t Bool
          )
     )
  -> InputConfig t m (ComboboxValue (Maybe k))
  -> m (DomInputEl t m (ComboboxValue (Maybe k)), b)
comboboxInputKS' before' setDynSelection showOpt initOpts mkSetOptsEv cfg =
  comboboxInputWrapper
    before'
    (comboboxInputRawInput setDynSelection showOpt initOpts mkSetOptsEv cfg)


comboboxInputRawInput
  :: forall t m k js
   . ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , Ord k
     , Prerender js t m
     )
  => Event t (ComboboxValue (Maybe k))
  -> (Dynamic t k -> Dynamic t Text -> Client m ())
  -> MapSubset Int (k, Text)
  -> (  Event t (OptionsRequest k)
     -> m
          ( Event t (Either Text (MapSubset Int (k, Text)))
          , Dynamic t Bool
          )
     )
  -> InputConfig t m (ComboboxValue (Maybe k))
  -> m
       ( DomInputEl t m (ComboboxValue (Maybe k))
       , Dynamic t InputStatus
       )
comboboxInputRawInput setDynSelection showOpt initOpts mkSetOptsEv cfg = do
  rec
    searchStrInput <- textInputEl (_cb_text <$> cfg)
      { _inputConfig_setValue         = leftmost
        [_inputConfig_setValue (_cb_text <$> cfg), setTextEv]
      , _inputConfig_attributes       = _inputConfig_attributes cfg
                                        <> maybe
                                             mempty
                                             (Map.singleton "list" . (<> "-datalist"))
                                             (_inputConfig_id cfg)
                                        <> "class"
                                        =: "combobox"
      , _inputConfig_modifyAttributes = mergeWith
                                          (<>)
                                          [ _inputConfig_modifyAttributes cfg
                                          , setOpenAttrEv
                                          ]
      , _inputConfig_status           = dynStatusFull
      , _inputConfig_eventSpec        = addEventSpecFlags
                                          (Nothing :: Maybe (DomBuilderSpace m))
                                          Keydown
                                          preventArrowDef
      }

    selectIcon

    (e, (dynOptEv', pageEv, pageDyn)) <-
      elDynAttr' "div" (mkDatalistAttr <$> openDyn) $ do
        elDynClass
            "i"
            (   (\c loading -> mkVisible (c == Just 0 && Prelude.not loading))
            <$> dynCount
            <*> loadingDyn
            )
          $ text "No results found"
        r' <-
          prerender
              (pure (constDyn (0, 0), (constDyn (0, 0), constDyn Map.empty)))
            $ virtualListMaxHeightBuffered bufferSize
                                           252
                                           24
                                           dynCountWithDef
                                           0
                                           scrollEv
                                           Prelude.id
                                           (_ms_data initOpts)
                                           (_ms_data <$> updateRows)
                                           (mkOption sdemux)
        let r = snd . snd =<< r'
        let pageEv' =
              fmap (const pageSize) <$> switchDyn (updated . fst <$> r')
        let pageDyn' = fmap (const 10) <$> (fst . snd =<< r')
        elDynClass "i" (mkVisible <$> loadingDyn)
          $ spinner Inline "Searching for matches"
        pure (r, pageEv', pageDyn')

    let selectedIndexEv = maybe 0 fst . Map.lookupMin <$> switchDyn
          (mergeMap . fmap fst <$> dynOptEv')

-- This keeps the dropdown open while the user is clicking an item, even though the input has lost focus
    mousePressed <- holdDyn False
      $ leftmost [True <$ domEvent Mousedown e, False <$ domEvent Click e]

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

-- Tab completion
    let inputEls      = _inputEl_elements searchStrInput
    let keydownEv = switchDyn (leftmost . fmap (domEvent Keydown) <$> inputEls)
    let arrowUpDownEv = ffilter (`elem` 9 : arrowKeys) keydownEv
-- The updated _inputEl_value occurs later than the input event
-- We want to listen only to the input event, but with the current entered text
-- Therefore, we store if the next updated _inputEl_value was a keyboard input event
    isInputDyn <- hold False $ leftmost
      [ True <$ switchDyn (leftmost . fmap (domEvent Input) <$> inputEls)
      , False <$ updated (_inputEl_value searchStrInput)
      ]
    let searchEv = gate isInputDyn (updated (_inputEl_value searchStrInput))
    searchText <- holdDyn "" searchEv

-- Whenever the current selection is not in the list any more, clear it
    let setOpenEv = False <$ ffilter (== 13) arrowUpDownEv

    hasFocusDyn <-
      holdUniqDyn $ (||) <$> _inputEl_hasFocus searchStrInput <*> mousePressed

    let dynOptions = joinDynThroughMap (fmap snd <$> dynOptEv')

    let scrollEv = attachWithMaybe jumpToSelected
                                   (current pageDyn)
                                   (updated selectedIndex)

    let reqEv = leftmost
          [ (`NearSelection` (pageSize + 4)) . fmap fst <$> tag
            (current dynSelection)
            (ffilter Prelude.id (updated hasFocusDyn))
          , (`SearchText` (0, pageSize)) <$> searchEv
          , attachWith
            SearchText
            (current searchText)
            (attachWithMaybe needsNewPage
                             (current dynOptions)
                             (gate (current hasFocusDyn) pageEv)
            )
          ]

    (setOptsEvResult, loadingDyn) <- mkSetOptsEv reqEv

    let (setOptsEvErr, setOptsEv) = fanEither setOptsEvResult

    dynError <- holdDyn def
      $ leftmost [InputError <$> setOptsEvErr, def <$ setOptsEv]

    let updateRows =
          attachWith addDeletes (Map.keysSet <$> current dynOptions) setOptsEv
    dynCount <- holdDyn (_ms_totalCount initOpts) (_ms_totalCount <$> setOptsEv)
    dynCountWithDef <- holdDyn (totalCountOrSize initOpts)
                               (totalCountOrSize <$> setOptsEv)

-- Clear the selection if the text is null
    let autofillExact =
          attachWithMaybe (flip findExactMatch) (current dynOptions) searchEv

-- Update the text on lose focus if a value has been selected
    let
      lostFocusEv =
        gate
            (current (Prelude.not . Text.null <$> _inputEl_value searchStrInput)
            )
          $ ffilter Prelude.not (updated hasFocusDyn)
    let selectedIndexWithValEv =
          attachWith (Map.!?) (current dynOptions) selectedIndexEv
    let arrowUpDownWithValEv = attachWithMaybe
          selectNext
          ((,) <$> current dynOptions <*> current selectedIndex)
          arrowUpDownEv
    let setTextEv = fmapMaybe (fmap snd) $ leftmost
          [ tag (current dynSelection) lostFocusEv
          , arrowUpDownWithValEv
          , selectedIndexWithValEv
          ]


    selectedIndex' <- holdDyn (findIndex initialKey initOpts) $ leftmost
      [ attachWithMaybe findNewIndex
                        (current (fmap fst <$> dynSelection))
                        setOptsEv
      , attachWith (flip findIndex')
                   (current dynOptions)
                   (fmap fst <$> updated dynSelection)
      ]
    selectedIndex <- holdUniqDyn selectedIndex'

    let sdemux = demux selectedIndex

    dynSelection <- holdDyn (toPair $ _inputConfig_initialValue cfg) $ leftmost
      [ toPair <$> setDynSelection
      , attachWithMaybe fillOnSetOpts (current searchText) setOptsEv
      , Nothing <$ ffilter Text.null (updated (_inputEl_value searchStrInput))
      , selectedIndexWithValEv
      , arrowUpDownWithValEv
      , Just <$> autofillExact
      ]

-- Show an error if selection is null and the value is not null, and show it on lose focus
    let dynStatus =
          mkStatus
            <$> _inputEl_value searchStrInput
            <*> (fmap fst <$> dynSelection)
            <*> hasFocusDyn

    let dynStatusFull = dynError <> _inputConfig_status cfg <> dynStatus

    let comboVal =
          ComboboxValue
            <$> (fmap fst <$> dynSelection)
            <*> _inputEl_value searchStrInput
    comboValUniq <- holdUniqDyn comboVal

  pure (InputEl comboValUniq hasFocusDyn inputEls, dynStatusFull)
 where
  fillOnSetOpts curSearchText setOpts
    | _ms_totalCount setOpts == Just 0
    = Just Nothing
    | -- Clear if no options present
      _ms_totalCount setOpts == Just 1 && Prelude.not (Text.null curSearchText)
    = fmap (Just . snd) . Map.lookupMin $ _ms_data setOpts
    |  -- Try to autofill if there is only one option
      otherwise
    = Just <$> findExactMatch curSearchText (_ms_data setOpts)

  needsNewPage m pg@(o, l) =
    case (fst <$> Map.lookupMin m, fst <$> Map.lookupMax m) of
      (Just o', Just ol') ->
        if o' <= o && o + l <= ol' then Nothing else Just pg
      _ -> Just pg

  toPair x = fmap (, _cb_text x) (_cb_selection x)

  -- If outide the range jump to the selected index
  jumpToSelected (voff, _) Nothing = Just voff
  jumpToSelected (voff, vlim) (Just ind)
    | voff <= ind && ind < voff + vlim = Nothing
    | otherwise = Just (Prelude.max 0 (ind - (vlim `Prelude.div` 2)))

  bufferSize = 5
  pageSize   = bufferSize * 10

  mkStatus :: Text -> Maybe k -> Bool -> InputStatus
  mkStatus x Nothing False
    | Prelude.not (Text.null x)
    = InputError $ "The option " <> x <> " could not be found."
    | otherwise
    = InputNeutral Nothing
  mkStatus _ _ _ = InputNeutral Nothing

  initialKey = _cb_selection $ _inputConfig_initialValue cfg
  findIndex k (MapSubset d _) = findIndex' k d

  -- Only clear the selection if the count is 0
  findNewIndex k (MapSubset d (Just 0)) = Just (findIndex' k d)
  findNewIndex k (MapSubset d _       ) = fmap Just (findIndex' k d)

  findIndex' (Just k) d =
    fmap fst . Map.lookupMin $ Map.filter ((== k) . fst) d
  findIndex' Nothing _ = Nothing

  findExactMatch "" _ = Nothing
  findExactMatch k  d = fmap snd . Map.lookupMin $ Map.filter
    ((== Text.toCaseFold k) . Text.toCaseFold . snd)
    d

  selectNext _ 13 = Nothing
  selectNext (m, Just s') key =
    let offs = case key of
          38 -> -1
          40 -> 1
          _  -> 0
    in  fmap Just $ m Map.!? (s' + offs)
  selectNext (m, Nothing) key =
    let offs = case key of
          38 -> Map.lookupMax
          40 -> Map.lookupMin
          _  -> const Nothing
    in  Just . snd <$> offs m

  mkVisible True  = ""
  mkVisible False = "hidden"

  mkOption sdemux attrs ind initOpt setOptEv = do
    dynOpt <- holdDyn initOpt setOptEv
    let (dynK, dynV) = splitDynPure dynOpt
    let dynSelection = demuxed sdemux (Just ind)

    (e, _) <- elDynAttr'
      "div"
      (((attrs <>) . ("value" =:) <$> dynV) <> (mkCurrentCls <$> dynSelection))
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
     , Prerender js t m
     )
  => (a -> Text)
  -> InputConfig' (OpElem (Client m)) t m (Maybe a)
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
     , Prerender js t m
     )
  => InputConfig' (OpElem (Client m)) t m (Maybe a)
  -> m (DomInputEl t m (Maybe a))
altSelectInput = altSelectInput' (Text.pack . show)

multiComboboxInput
  :: forall t m k js
   . ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , Ord k
     , Prerender js t m
     )
  => (Dynamic t k -> Dynamic t Text -> Client m ())
  -> Map k Text
  -> InputConfig t m (ComboboxValue (Set k))
  -> m (DomInputEl t m (ComboboxValue (Set k)))
multiComboboxInput showOpt allOptions cfg = do
  rec (r, dEv) <- comboboxInputKS'
        (before' dynValue)
        never
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

      let addOnChange = attachWithMaybe
            const
            (current selectionDyn)
            (ffilter Prelude.not (updated (_inputEl_hasFocus r)))

      let enabledDyn = current ((/= InputDisabled) <$> _inputConfig_status cfg)

      dynValue <-
        foldDyn ($) (_cb_selection $ _inputConfig_initialValue cfg) $ leftmost
          [ gate enabledDyn $ Set.insert <$> addOnChange
          , gate enabledDyn
                 ((\m xs -> Set.difference xs (Map.keysSet m)) <$> dEv)
          , const . _cb_selection <$> _inputConfig_setValue cfg
          ]
  pure $ r { _inputEl_value = ComboboxValue <$> dynValue <*> searchText }
 where
  before' dynValues =
    listViewWithKey (Map.restrictKeys allOptions <$> dynValues) $ \_ v -> do
      tagEl def { _tagConfig_action = Just TagDismiss } v

-- | Modified from @Reflex.Dom.Widget.Lazy.virtualList@, version which can shrink
-- if there are less options than the available size
-- This will have a fixed buffer size though
virtualListMaxHeight
  :: forall t m k v a
   . ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , Ord k
     )
  => Int -- ^ The maximum available visible region's height in pixels
  -> Int -- ^ The fixed height of each row in pixels
  -> Dynamic t Int -- ^ A 'Dynamic' of the total number of items
  -> Int -- ^ The index of the row to scroll to on initialization
  -> Event t Int -- ^ An 'Event' containing a row index. Used to scroll to the given index.
  -> (k -> Int) -- ^ Key to Index function, used to position items.
  -> Map k v -- ^ The initial 'Map' of items
  -> Event t (Map k (Maybe v)) -- ^ The update 'Event'. Nothing values are removed from the list and Just values are added or updated.
  -> (Map Text Text -> k -> v -> Event t v -> m a) -- ^ The row child element builder.
  -> m
       ( Dynamic t (Int, Int)
       , ( Dynamic t (Int, Int)
         , Dynamic t (Map k a)
         )
       ) -- ^ A tuple containing: a 'Dynamic' of the index (based on the current scroll position) and number of items currently being rendered, and the 'Dynamic' list result
virtualListMaxHeight heightPx rowPx maxIndex i0 setI' keyToIndex items0 itemsUpdate itemBuilder
  = do
    let virtualH       = mkVirtualHeight <$> maxIndex
        minHeightPx    = Prelude.min heightPx . (* rowPx) <$> maxIndex
        containerStyle = fmap mkContainer minHeightPx
        viewportStyle  = fmap mkViewport minHeightPx
    pb   <- getPostBuild
    setI <- delay 0.2 setI'
    rec (viewport, result) <-
          elDynAttr "div" containerStyle
          $ elDynAttr' "div" viewportStyle
          $ elDynAttr "div" virtualH
          $ listWithKeyShallowDiff items0 itemsUpdate
          $ \k v e -> itemBuilder (mkRow k) k v e
        scrollPosition <- holdDyn 0 $ leftmost
          [ Prelude.round <$> domEvent Scroll viewport
          , fmap (const (i0 * rowPx)) pb
          ]
        let window = findWindow rowPx heightPx <$> scrollPosition
    performEvent_ $ ffor (leftmost [setI, i0 <$ pb]) $ \i' ->
      setScrollTop (_element_raw viewport) (i' * rowPx)
    uniqWindow <- holdUniqDyn window
    return (uniqWindow, (uniqWindow, result))
 where
  toStyleAttr m = Map.singleton "style"
    $ Map.foldrWithKey (\k v kv -> k <> ":" <> v <> ";" <> kv) "" m
  mkViewport h = toStyleAttr $ Map.fromList
    [ ("overflow", "auto")
    , ("position", "absolute")
    , ("left"    , "0")
    , ("right"   , "0")
    , ("height"  , pack (show h) <> "px")
    ]
  mkContainer h = toStyleAttr $ Map.fromList
    [("position", "relative"), ("height", pack (show h) <> "px")]
  mkVirtualHeight h =
    let h' = h * rowPx
    in  toStyleAttr $ Map.fromList
          [ ("height"  , pack (show h') <> "px")
          , ("overflow", "hidden")
          , ("position", "relative")
          ]
  mkRow k = toStyleAttr $ Map.fromList
    [ ("height"  , pack (show rowPx) <> "px")
    , ("top", (<> "px") (pack $ show $ keyToIndex k * rowPx))
    , ("position", "absolute")
    , ("width"   , "100%")
    ]

  findWindow sizeIncrement windowSize startingPosition =
    let (startingIndex, _) = startingPosition `divMod'` sizeIncrement
        numItems = (windowSize + sizeIncrement - 1) `Prelude.div` sizeIncrement
    in  (startingIndex, numItems)

virtualListMaxHeightBuffered
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , Ord k
     )
  => Int
  -> Int
  -> Int
  -> Dynamic t Int
  -> Int
  -> Event t Int
  -> (k -> Int)
  -> Map k v
  -> Event t (Map k (Maybe v))
  -> (Map Text Text -> k -> v -> Event t v -> m a)
  -> m
       ( Dynamic t (Int, Int)
       , ( Dynamic t (Int, Int)
         , Dynamic t (Map k a)
         )
       )
virtualListMaxHeightBuffered buffer heightPx rowPx maxIndex i0 setI keyToIndex items0 itemsUpdate itemBuilder
  = buffered buffer $ virtualListMaxHeight heightPx
                                           rowPx
                                           maxIndex
                                           i0
                                           setI
                                           keyToIndex
                                           items0
                                           itemsUpdate
                                           itemBuilder
