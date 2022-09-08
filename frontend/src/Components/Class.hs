{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Components.Class
  ( Status(..)
  , ComponentSize(..)
  , statusColor
  , borderRadiusAll
  , paddingAll
  , marginAll
  , absoluteBlock
  , spantext
  , WidgetConstraint
  , MapSubset(..)
  , addDeletes
  , universe
  , totalCountOrSize
  , buffered
  )
where


import           Clay
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           Reflex.Dom

import           Nordtheme


data Status = Danger -- ^ Show errors to make user pause and evaluate
            | Warning  -- ^ Proceed with caution
            | Info  -- ^ Inform the user
            | Success -- ^ Let the user know something is correct
            deriving (Eq, Ord, Show)

instance Default Status where
  def = Info

statusColor :: Status -> Color
statusColor = \case
  Danger  -> nord11'
  Warning -> nord13'
  Info    -> nord10'
  Success -> green1'

data ComponentSize = NormalSize | CompactSize deriving (Eq, Ord, Show)

instance Default ComponentSize where
  def = NormalSize


borderRadiusAll :: Size a -> Css
borderRadiusAll x = borderRadius x x x x

paddingAll :: Size a -> Css
paddingAll x = padding x x x x

marginAll :: Size a -> Css
marginAll x = margin x x x x

absoluteBlock :: Css
absoluteBlock = do
  content (stringContent "")
  Clay.display block
  position absolute

spantext :: (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m ()
spantext = el "span" . dynText

type WidgetConstraint t m
  = ( MonadFix m
    , DomBuilder t m
    , PostBuild t m
    , TriggerEvent t m
    , PerformEvent t m
    , MonadHold t m
    , MonadIO (Performable m)
    , Prerender t m
    )

-- | A subset of a map of known or unknown size.
-- Used to model requesting a page (offset and limit) of a dataset,
-- together with the total number of records in the dataset.
-- Differs from a set in that each element is indexed by a key k, usually the offset position.
data MapSubset k a = MapSubset
  { _ms_data       :: !(Map k a)
  , _ms_totalCount :: !(Maybe Integer)
  }

-- | An empty subset of a set of unknown size
instance Default (MapSubset k a) where
  def = MapSubset Map.empty Nothing

instance Functor (MapSubset k) where
  fmap f x = x { _ms_data = fmap f (_ms_data x) }

-- | Set union, with two distinct universes
instance Ord k => Semigroup (MapSubset k a) where
  x <> y = MapSubset
    { _ms_data       = _ms_data x <> _ms_data y
    , _ms_totalCount = (+) <$> _ms_totalCount x <*> _ms_totalCount y
    }

-- | Empty universe
instance Ord k => Monoid (MapSubset k a) where
  mempty = universe Map.empty

-- | The entire map as a MapSubset, so it's size is known
universe :: Map k a -> MapSubset k a
universe xs = MapSubset { _ms_data       = xs
                        , _ms_totalCount = Just . fromIntegral $ Map.size xs
                        }


addDeletes :: Ord k => Set k -> MapSubset k a -> MapSubset k (Maybe a)
addDeletes oldSet m = m { _ms_data = diffMapSet oldSet (_ms_data m) }

diffMapSet :: (Ord k) => Set k -> Map k v -> Map k (Maybe v)
diffMapSet olds news = fmap Just news
  `Map.union` Map.fromSet
                (const Nothing)
                (Set.difference olds (Map.keysSet news))

totalCountOrSize :: MapSubset k a -> Int
totalCountOrSize (MapSubset _  (Just x)) = fromIntegral x
totalCountOrSize (MapSubset xs _       ) = Map.size xs

-- | Helper for virtualListBuffered, modified from @Reflex.Dom.Wigdet.Lazy.virtualListBuffered@
buffered
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Int
  -> m (Dynamic t (Int, Int), a)
  -> m (Dynamic t (Int, Int), a)
buffered buffer vList = do
  (win, m) <- vList
  pb       <- getPostBuild
  let extendWin o l =
        (Prelude.max 0 (o - l * (buffer - 1) `Prelude.div` 2), l * buffer)
  rec let winHitEdge = attachWithMaybe
            (\(oldOffset, oldLimit) (winOffset, winLimit) ->
              if winOffset
                 >  oldOffset
                 && winOffset
                 +  winLimit
                 <  oldOffset
                 +  oldLimit
              then
                Nothing
              else
                Just (extendWin winOffset winLimit)
            )
            (current winBuffered)
            (updated win)
      winBuffered <- holdDyn (0, 0) $ leftmost
        [winHitEdge, attachPromptlyDynWith (\(x, y) _ -> extendWin x y) win pb]
  return (winBuffered, m)
