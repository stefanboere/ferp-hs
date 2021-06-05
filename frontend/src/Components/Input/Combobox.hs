{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Components.Input.Combobox
  ( comboboxStyle
  , ComboboxValue(..)
  , comboboxInput
  )
where

import           Prelude                 hiding ( rem
                                                , (**)
                                                )

import           Clay                    hiding ( icon )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Default
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Text.Fuzzy
import           Reflex.Dom              hiding ( (&)
                                                , display
                                                )

import           Components.Input.Basic
import           Nordtheme

comboboxStyle :: Css
comboboxStyle = ".combobox-menu" ? do
  display block
  important $ top (rem (7 / 4))
  width (pct 100)

  option ? do
    Clay.display flex
    justifyContent spaceBetween
    alignItems center
    paddingLeft (rem 1)
    height (rem (3 / 2))
    textTransform none
    backgroundColor inherit
    color nord3'
    "fill" -: showColor nord3'
    cursor pointer

    ".active" Clay.& do
      background nord4'
      hover Clay.& backgroundColor nord4'

    hover Clay.& background nord6'

data ComboboxValue k = ComboboxValue
  { _cb_selection :: k
  , _cb_text :: Text
  }
  deriving (Show, Eq)

instance Default k => Default (ComboboxValue k) where
  def = ComboboxValue def mempty

comboboxInput
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, Eq k)
  => (Dynamic t k -> Dynamic t Text -> m ())
  -> Dynamic t (Map k Text)
  -> InputConfig t (ComboboxValue (Maybe k))
  -> m (DomInputEl t m (ComboboxValue (Maybe k)))
comboboxInput showOpt allOptions cfg = do
  rec
    (searchStrInput, selectEv) <- textInputWithIco'
      (after' dynSelection options hasFocusDyn)
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
          [_inputConfig_setValue (_cb_text <$> cfg), setTextEv]
        , _inputConfig_status     = _inputConfig_status cfg <> dynStatus
        }
    let hasFocusDyn = _inputEl_hasFocus searchStrInput

-- Filter possible keys with fuzzyfind
    let options =
          filterOptions <$> _inputEl_value searchStrInput <*> allOptions

-- Clear the selection if the text is null
    let clearEv    = ffilter Text.null $ updated (_inputEl_value searchStrInput)

-- Tab completion
    let inputEls   = _inputEl_elements searchStrInput

-- Whenever the current selection is not in the list any more, clear it
    let autofillEv = fmap autofill (updated options)

-- Update the text on lose focus if a value has been selected
    let
      lostFocusEv =
        gate
            (current (Prelude.not . Text.null <$> _inputEl_value searchStrInput)
            )
          $ ffilter Prelude.not (updated hasFocusDyn)
    let selectOnLostFocusEv =
          fmapMaybe (fmap snd . Map.lookupMin)
            $ tagPromptlyDyn options lostFocusEv
    let setTextEv = snd <$> selectOnLostFocusEv

    dynSelection <- holdDyn Nothing $ leftmost
      [ Just <$> selectEv
      , Just . fst <$> selectOnLostFocusEv
      , Nothing <$ clearEv
      , autofillEv
      ]

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

  autofill opts = fst . snd <$> Map.lookupMin opts

  filterOptions :: Text -> Map k Text -> Map Integer (k, Text)
  filterOptions pat opts =
    let r = Text.Fuzzy.filter pat (Map.toList opts) "" "" snd False
    in  Map.fromList $ zip [1, 0 ..] (Prelude.map original r)

  after' dynSelection options hasFocusDyn = do
    selectIcon

    rec (e, dynOptEv) <-
          elDynAttr' "datalist" (mkDatalistAttr <$> openDyn)
            $ listViewWithKey options (mkOption dynSelection)

        -- This keeps the dropdown open while the user is clicking an item, even though the input has lost focus
        mousePressed <- holdDyn False
          $ leftmost [True <$ domEvent Mousedown e, False <$ domEvent Mouseup e]

        openDyn <- holdDyn False $ leftmost
          [ gate (Prelude.not <$> current mousePressed) (updated hasFocusDyn)
          , False <$ dynOptEv
          ]

    let
      setOpenAttrEv =
        (\isOpen ->
            Map.singleton "class" (if isOpen then Just "open" else Nothing)
          )
          <$> updated openDyn

    let selectedKeyEv = snd . Map.findMin <$> dynOptEv

    pure (fst <$> selectedKeyEv, (setOpenAttrEv, snd <$> selectedKeyEv))

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
