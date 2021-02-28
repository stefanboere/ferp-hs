{-# LANGUAGE OverloadedStrings #-}
module Components.Card
  ( card
  , clickableCard
  , cardStyle
  , cardHeader
  , cardFooter
  , cardContent
  , cardAction
  )
where

import           Prelude                 hiding ( rem )

import           Clay
import           Data.Text                      ( Text )
import           Reflex.Dom              hiding ( display
                                                , (&)
                                                )

import           Components.Button
import           Components.Class
import           Nordtheme


cardStyle :: Css
cardStyle = ".card" ? do
  border solid (px 1) grey0'
  borderRadiusAll (px 3)
  backgroundColor white
  boxShadow . pure $ bsColor grey0' $ shadowWithBlur nil (rem (1 / 8)) nil

  star # Clay.not ".card-footer" <? do
    paddingAll (rem (3 / 4))
    borderBottom solid (px 1) grey0'

    lastChild & borderBottomWidth nil

  ".card-header" ? marginTop nil

  ".card-content" <? (h1 <> h2 <> h3 <> h4 <> h5 <> h5) # firstOfType ? do
    marginTop nil
    marginBottom (rem (1 / 2))


card :: DomBuilder t m => m a -> m a
card = elClass "div" "card"

clickableCard :: DomBuilder t m => m () -> m (Event t ())
clickableCard cnt = do
  cnt
  pure never

cardHeader :: DomBuilder t m => m a -> m a
cardHeader = elClass "h3" "card-header"

cardContent :: DomBuilder t m => m a -> m a
cardContent = elClass "div" "card-content"

cardFooter :: DomBuilder t m => m a -> m a
cardFooter = elClass "div" "card-footer"

cardAction
  :: (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m (Event t ())
cardAction = btn def { _buttonConfig_priority = ButtonTertiary } . dynText
