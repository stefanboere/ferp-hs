{-# LANGUAGE OverloadedStrings #-}
module Frontend.Crud.Utils
  ( triStateBtn
  , backBtn
  , saveBtn
  , refreshBtn
  , deleteBtn
  , insertBtn
  , deleteConfirmation
  , messageBox
  ) where

import           Control.Monad.Fix              ( MonadFix )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )

import           Components


triStateBtn
  :: (PostBuild t m, DomBuilder t m)
  => m ()
  -> (Text, Text, Text)
  -> Dynamic t ActionState
  -> m (Event t ())
triStateBtn ico (x, xing, xed) stateDyn =
  btn def { _buttonConfig_state    = stateDyn
          , _buttonConfig_priority = ButtonSecondary
          }
    $  icon def ico
    >> el "span" (dynText (stateText <$> stateDyn))
 where
  stateText ActionAvailable = x
  stateText ActionError     = x
  stateText ActionLoading   = xing
  stateText ActionSuccess   = xed
  stateText ActionDisabled  = xed

backBtn :: (PostBuild t m, DomBuilder t m) => m (Event t ())
backBtn =
  btn def { _buttonConfig_priority = ButtonSecondary }
    $  icon def timesIcon
    >> el "span" (text "Close")

saveBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
saveBtn = triStateBtn floppyIcon ("Save", "Saving", "Saved")

refreshBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
refreshBtn = triStateBtn refreshIcon ("Reload", "Loading", "Loaded")

insertBtn :: (PostBuild t m, DomBuilder t m) => m (Event t ())
insertBtn =
  btn def { _buttonConfig_priority = ButtonSecondary } $ icon def plusIcon >> el
    "span"
    (text "Insert")

deleteBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
deleteBtn = triStateBtn trashIcon ("Delete", "Deleting", "Delete")

deleteConfirmation
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => Event t [a]
  -> m (Event t [a])
deleteConfirmation = messageBox
  "Confirm deletion"
  (getMsg . length)
  (btn def { _buttonConfig_priority = ButtonPrimary Danger } (text "Delete"))

 where
  getMsg 1 = "Are you sure you want to delete this item?"
  getMsg l =
    "Are you sure you want to delete these " <> pack (show l) <> " items?"

messageBox
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => Text
  -> (a -> Text)
  -> m (Event t ())
  -> Event t a
  -> m (Event t a)
messageBox titl msg okBtn openEv = fmapMaybe id
  <$> modal ModalMedium (modalContent <$> openEv)
 where
  modalContent y = card $ do
    x <- cardHeader (text titl >> modalCloseBtn)

    cardContent $ el "p" $ text (msg y)

    cardFooter $ do
      cancelEv <- cardAction "Cancel"
      okEv     <- okBtn
      pure $ leftmost [Nothing <$ x, Nothing <$ cancelEv, Just y <$ okEv]
