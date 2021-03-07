{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend.Container
  ( containerHandler
  , containerLinks
  , ContainerApi
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Proxy
import           URI.ByteString
import           Reflex
import           Reflex.Dom              hiding ( rangeInput
                                                , Link(..)
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router

import           Components

-- brittany-disable-next-binding
type ContainerApi = "container" :> "accordion" :> View
        :<|> "container" :> "card" :> View
        :<|> "container" :> "tab" :> View
        :<|> "container" :> "table" :> View

containerApi :: Proxy ContainerApi
containerApi = Proxy

containerAccordionLink, containerCardLink, containerTabLink, containerTableLink
  :: Link
containerAccordionLink :<|> containerCardLink :<|> containerTabLink :<|> containerTableLink
  = allLinks containerApi

containerLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t ())
containerLinks dynUri = safelinkGroup
  (text "Containers")
  [ safelink dynUri containerAccordionLink $ text "Accordion"
  , safelink dynUri containerCardLink $ text "Card"
  , safelink dynUri containerTabLink $ text "Tab"
  , safelink dynUri containerTableLink $ text "Table"
  ]

containerHandler :: MonadWidget t m => RouteT ContainerApi m (Event t URI)
containerHandler =
  containerAccordion :<|> containerCard :<|> containerTab :<|> containerTable

containerAccordion
  :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
containerAccordion = do
  el "h1" $ text "Accordion"
  accordion never
            "Header for panel #1"
            (text "This is the content for accordion panel #1")
  accordion never
            "Header for panel #2"
            (text "This is the content for accordion panel #2")
  accordion never
            "Header for panel #3"
            (text "This is the content for accordion panel #3")
  pure never

containerCard :: (PostBuild t m, DomBuilder t m) => m (Event t URI)
containerCard = do
  el "h1" $ text "Card"
  _ <- card $ do
    cardHeader (text "Header")

    cardContent $ do
      _ <- alert
        def { _alertConfig_size = CompactSize, _alertConfig_status = Warning }
        "Use small alerts in a card."
        (pure ())
      el "h4" $ text "Block"
      text "Card content"

    cardContent $ el "ul" $ do
      el "li" $ text "Ullamco Laboris"
      el "li" $ do
        text "Nisi Ut Aliquip"
        el "ul" $ do
          el "li" $ text "Exercitation"
          el "li" $ text "Laboris"
          el "li" $ text "Commodo"
      el "li" $ text "Consequat"

    cardFooter $ do
      x <- cardAction "Footer action 1"
      y <- cardAction "Footer action 2"
      pure (x, y)

  elClass "div" "grid" $ do
    clickableCard "#" $ do
      cardImg $ elAttr
        "img"
        ("src" =: "https://via.placeholder.com/350x150?text=Image")
        blank
      cardContent $ text "..."

    clickableCard "#" $ do
      cardContent $ text "..."
      cardImg $ elAttr
        "img"
        ("src" =: "https://via.placeholder.com/350x150?text=Image")
        blank

    clickableCard "#" $ cardImg $ elAttr
      "img"
      ("src" =: "https://via.placeholder.com/350x150?text=Image")
      blank


  pure never

containerTab
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
containerTab = do
  el "h1" $ text "Tab"
  tabs
    (  1
    =: ("Tab 1", text "Tab 1 content")
    <> (2 :: Int)
    =: ("Tab 2", text "Tab 2 content")
    )
  pure never

containerTable
  :: (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => m (Event t URI)
containerTable = do
  el "h1" $ text "Table"
  _ <- tableDyn
    [ ("First" , \_ r -> dynText (fst <$> r))
    , ("Second", \_ r -> dynText (snd <$> r))
    ]
    (constDyn
      (1 =: ("First row", "Foo bar") <> (2 :: Int) =: ("Second row", "Bazz"))
    )
  pure never

