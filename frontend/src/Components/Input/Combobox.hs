{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Components.Input.Combobox
  ( comboboxStyle
  , ComboboxValue(..)
  , comboboxInput
  , comboboxInput'
  )
where

import           Prelude                 hiding ( rem
                                                , (**)
                                                )

import           Clay                    hiding ( icon )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Default
import           Data.Map                       ( Map )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Reflex.Dom              hiding ( (&)
                                                , display
                                                )

import           Components.Icon
import           Components.Input.Basic
import           Nordtheme

comboboxStyle :: Css
comboboxStyle = do
  ".combobox-menu" ? do
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


      hover Clay.& do
        background nord6'

data ComboboxValue k = ComboboxValue
  { _cb_selection :: k
  , _cb_text :: Text
  }
  deriving (Show, Eq)

instance Default k => Default (ComboboxValue k) where
  def = ComboboxValue def mempty

comboboxInput
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , MonadIO m
     , Ord k
     )
  => (k -> Dynamic t Text -> m ())
  -> Dynamic t (Map k Text)
  -> InputConfig t (ComboboxValue (Maybe k))
  -> m (Dynamic t (ComboboxValue (Maybe k)))
comboboxInput showOpt options cfg =
  _inputEl_value <$> labeled cfg (\x -> comboboxInput' x showOpt options)

comboboxInput'
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m, Ord k)
  => Text
  -> (k -> Dynamic t Text -> m ())
  -> Dynamic t (Map k Text)
  -> InputConfig t (ComboboxValue (Maybe k))
  -> m (InputEl t (ComboboxValue (Maybe k)))
comboboxInput' idStr showOpt options cfg = do
  rec searchStrInput <- textInput'
        (after' hasFocusDyn)
        idStr
        (_cb_text <$> cfg)
          { _inputConfig_attributes = _inputConfig_attributes cfg
                                      <> "list"
                                      =: listIdStr
                                      <> "class"
                                      =: "combobox"
          }
      let hasFocusDyn = _inputEl_hasFocus searchStrInput
  pure $ ComboboxValue Nothing <$> searchStrInput
 where
  listIdStr = idStr <> "-datalist"
  after' hasFocusDyn = do
    selectIcon

    rec (e, dynOptEv) <-
          elDynAttr' "datalist" (mkDatalistAttr <$> openDyn)
            $ listViewWithKey options mkOption

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

    let updateValAttrEv =
          fmap (fromMaybe "")
            $   attachPromptlyDynWith (Map.!?) options
            $   fst
            .   Map.findMin
            <$> dynOptEv

    pure (setOpenAttrEv, updateValAttrEv)

  mkOption k v = do
    (e, _) <- elDynAttr' "option" (("value" =:) <$> v) (showOpt k v)
    pure $ k <$ domEvent Click e

  mkDatalistAttr isOpen = Map.fromList
    [ ("id"   , listIdStr)
    , ("class", "combobox-menu" <> if isOpen then " open" else "")
    ]

