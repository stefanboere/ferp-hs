{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Components.History
  ( undoRedo
  ) where

import           Control.Monad.Fix              ( MonadFix )
import           Reflex.Dom

import           Components.Button
import           Components.Icon

-- | Provides basic undo and redo functionality
undoRedo
  :: (Eq a, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => a
  -> Event t a
  -> m (Event t a)
undoRedo initVal update = elClass "div" "button-group" $ do

  rec history <- foldDynMaybe
        ($)
        (emptyHistory initVal)
        (leftmost [put <$> update, const . Just <$> setHistEv])

      undoEv <- btn
        def { _buttonConfig_state    = avail hist_undos history
            , _buttonConfig_priority = ButtonSecondary
            }
        (icon def undoIcon)
      redoEv <- btn
        def { _buttonConfig_state    = avail hist_redos history
            , _buttonConfig_priority = ButtonSecondary
            }
        (icon def redoIcon)

      let setUndoEv = undo <$> tag (current history) undoEv
      let setRedoEv = redo <$> tag (current history) redoEv

      let setHistEv = fmapMaybe id (leftmost [setUndoEv, setRedoEv])

  pure $ hist_current <$> setHistEv

 where
  avail fn dynH = mkAvail . null . fn <$> dynH
  mkAvail True  = ActionDisabled
  mkAvail False = ActionAvailable


data History a = History
  { hist_undos   :: [a]
  , hist_current :: a
  , hist_redos   :: [a]
  }
  deriving (Eq, Show)

emptyHistory :: a -> History a
emptyHistory x = History [] x []

put :: Eq a => a -> History a -> Maybe (History a)
put x h
  | x == hist_current h = Nothing
  | otherwise = Just $ History
    { hist_undos   = take 20 (hist_current h : hist_undos h)
    , hist_current = x
    , hist_redos   = []
    }

undo :: History a -> Maybe (History a)
undo (History (x : xs) c rs) = Just $ History xs x (c : rs)
undo _                       = Nothing

redo :: History a -> Maybe (History a)
redo (History xs c (r : rs)) = Just $ History (c : xs) r rs
redo _                       = Nothing

