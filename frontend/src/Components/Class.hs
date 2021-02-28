{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Components.Class
  ( Status(..)
  , ComponentSize(..)
  , statusColor
  , borderRadiusAll
  , paddingAll
  , marginAll
  , absoluteBlock
  , spantext
  )
where


import           Clay
import           Data.Default
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
