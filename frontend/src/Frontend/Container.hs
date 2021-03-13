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
import           Data.Text                      ( pack )
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
        :<|> "container" :> "modal" :> View
        :<|> "container" :> "tab" :> View
        :<|> "container" :> "table" :> View

containerApi :: Proxy ContainerApi
containerApi = Proxy

containerAccordionLink, containerCardLink, containerModalLink, containerTabLink, containerTableLink
  :: Link
containerAccordionLink :<|> containerCardLink :<|> containerModalLink :<|> containerTabLink :<|> containerTableLink
  = allLinks containerApi

containerLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t ())
containerLinks dynUri = safelinkGroup
  (text "Containers")
  [ safelink dynUri containerAccordionLink $ text "Accordion"
  , safelink dynUri containerCardLink $ text "Card"
  , safelink dynUri containerModalLink $ text "Modal"
  , safelink dynUri containerTabLink $ text "Tab"
  , safelink dynUri containerTableLink $ text "Table"
  ]

containerHandler :: MonadWidget t m => RouteT ContainerApi m (Event t URI)
containerHandler =
  containerAccordion
    :<|> containerCard
    :<|> containerModal
    :<|> containerTab
    :<|> containerTable

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

containerModal
  :: (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m)
  => m (Event t URI)
containerModal = do
  el "h1" $ text "Modal"

  launchSmallEv <- btn def (text "Small")
  launchSmallCloseEv <- modal ModalSmall (modalContent <$ launchSmallEv)

  launchMediumEv <- btn def (text "Medium")
  launchMediumCloseEv <- modal ModalMedium (modalContent <$ launchMediumEv)

  launchLargeEv <- btn def (text "Large")
  launchLargeCloseEv <- modal ModalLarge (modalContent <$ launchLargeEv)

  launchXLEv <- btn def (text "X-Large")
  launchXLCloseEv <- modal ModalExtraLarge (modalContent <$ launchXLEv)

  (countDyn :: Dynamic t Integer) <- count $ leftmost
    [ launchSmallCloseEv
    , launchMediumCloseEv
    , launchLargeCloseEv
    , launchXLCloseEv
    ]
  el "p" $ dynText $ fmap (("Counter: " <>) . pack . show) countDyn

  pure never
 where
  modalContent = card $ do
    x <- cardHeader (text "Header" >> modalCloseBtn)

    cardContent $ el "p" $ text "This is the content of the modal"

    cardFooter $ do
      cancelEv <- cardAction "Cancel"
      okEv     <- btn def (text "Ok")
      pure $ leftmost [x, cancelEv, okEv]

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

