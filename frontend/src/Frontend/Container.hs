{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend.Container
  ( containerHandler
  , containerLinks
  , ContainerApi
  ) where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import qualified Data.Map                      as Map
import           Data.Proxy
import           Data.Text                      ( pack )
import           Data.Time                      ( fromGregorian )
import           Reflex
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router
import           URI.ByteString

import           Components

-- brittany-disable-next-binding
type ContainerApi = "container" :> "accordion" :> View
        :<|> "container" :> "card" :> View
        :<|> "container" :> "modal" :> View
        :<|> "container" :> "signpost" :> View
        :<|> "container" :> "tab" :> View
        :<|> "container" :> "table" :> View
        :<|> "container" :> "timeline" :> View
        :<|> "container" :> "treeview" :> View

containerApi :: Proxy ContainerApi
containerApi = Proxy

containerAccordionLink, containerCardLink, containerModalLink, containerSignpostLink, containerTabLink, containerTableLink, containerTimelineLink, containerTreeviewLink
  :: Link
containerAccordionLink :<|> containerCardLink :<|> containerModalLink :<|> containerSignpostLink :<|> containerTabLink :<|> containerTableLink :<|> containerTimelineLink :<|> containerTreeviewLink
  = allLinks containerApi

containerLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
containerLinks dynUri = safelinkGroup
  (text "Containers")
  [ safelink dynUri containerAccordionLink $ text "Accordion"
  , safelink dynUri containerCardLink $ text "Card"
  , safelink dynUri containerModalLink $ text "Modal"
  , safelink dynUri containerSignpostLink $ text "Signpost"
  , safelink dynUri containerTabLink $ text "Tab"
  , safelink dynUri containerTableLink $ text "Table"
  , safelink dynUri containerTimelineLink $ text "Timeline"
  , safelink dynUri containerTreeviewLink $ text "Treeview"
  ]

containerHandler
  :: WidgetConstraint js t m => RouteT ContainerApi m (Event t URI)
containerHandler =
  containerAccordion
    :<|> containerCard
    :<|> containerModal
    :<|> containerSignpost
    :<|> containerTab
    :<|> containerTable
    :<|> containerTimeline
    :<|> containerTreeview

containerAccordion
  :: (MonadIO m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
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

  el "h2" $ text "Stackview"
  el "div" $ do
    _ <- stackviewEmpty "Label 1" (textInput (inputConfig "Content 1"))
    _ <- stackview never (stackviewRow "Label 2" (text "Content 2")) $ do
      _ <- stackviewRow "Sub-label 1" (textInput (inputConfig "Sub-content 1"))
      stackviewRow "Sub-label 2" (text "Sub-content 2")
      stackviewRow "Sub-label 3" (text "Sub-content 3")
    _ <- stackview never (stackviewRow "Label 3" (text "Content 3")) $ do
      stackviewRow "Sub-label 1" (text "Sub-content 1")
      stackviewRow "Sub-label 2" (text "Sub-content 2")
      stackviewRow "Sub-label 3" (text "Sub-content 3")
    pure ()

  el "h2" $ text "Stepper"

  _ <- stepper $ do
    page1 <- stepperPage "Legal name" "Description goes here" $ do
      btnEv <- btn def { _buttonConfig_priority = ButtonSecondary }
                   (text "Next")
      pure (constDyn (), StepperSuccess <$ btnEv)

    page2 <- stepperPage "Contact information" "Description goes here" $ do
      btnEv <- btn def { _buttonConfig_priority = ButtonSecondary }
                   (text "Submit")
      pure (constDyn (), StepperError <$ btnEv)
    pure ((,) <$> page1 <*> page2)

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
  :: (MonadIO m, MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m)
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

  el "h2" $ text "Wizard"

  launchWizardMediumEv <- btn def (text "Medium")
  _ <- modal ModalMedium (wizardContent <$ launchWizardMediumEv)

  launchWizardLargeEv <- btn def (text "Large")
  _ <- modal ModalLarge (wizardContent <$ launchWizardLargeEv)

  launchWizardXLEv <- btn def (text "X-Large")
  _ <- modal ModalExtraLarge (wizardContent <$ launchWizardXLEv)


  pure never
 where
  wizardContent = wizard "Title" $ do
    step1 <- wizardPage "Legal name" $ do
      cardContent $ el "p" $ text "This is the content of the first page"

      (cancelEv, btnEv) <- modalFooter (btn def (text "Next"))
      pure (constDyn (), StepperSuccess <$ btnEv, cancelEv)

    step2 <- wizardPage "Contact information long title" $ do
      cardContent $ el "p" $ text "This is the content of the second page"

      (cancelEv, btnEv) <- modalFooter (btn def (text "Submit"))
      pure (constDyn (), StepperError <$ btnEv, cancelEv)
    pure $ (,) <$> step1 <*> step2

  modalContent = card $ do
    x <- cardHeader (text "Header" >> modalCloseBtn)

    cardContent $ el "p" $ text "This is the content of the modal"

    cardFooter $ do
      cancelEv <- cardAction "Cancel"
      okEv     <- btn def (text "Ok")
      pure $ leftmost [x, cancelEv, okEv]

containerSignpost
  :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m)
  => m (Event t URI)
containerSignpost = do
  el "h1" $ text "Signpost"
  el "p" $ do
    text "The default placement is right-top."
    mkSignpost def
  el "p" $ do
    text "The following placements are also possible."
    mapM_ mkSignpost [minBound .. maxBound]

  el "h2" $ text "Tooltip"
  tooltip TopRight $ constDyn "A tooltip can contain text content"

  pure never

 where
  mkSignpost pos = signpost pos $ do
    el "h3" $ text "Default signpost"
    el "p" $ text $ "Position: " <> pack (show pos)

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

  el "h2" $ text "Vertical tab"
  tabsVertical
    (  1
    =: ("Tab 1", text "Tab 1 content")
    <> (2 :: Int)
    =: ("Tab 2", text "Tab 2 content")
    )

  pure never

containerTable
  :: (MonadIO m, MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => m (Event t URI)
containerTable = do
  el "h1" $ text "Table"
  _ <- tableDyn
    [ (text "First" , \_ r -> dynText (fst <$> r))
    , (text "Second", \_ r -> dynText (snd <$> r))
    ]
    (constDyn
      (1 =: ("First row", "Foo bar") <> (2 :: Int) =: ("Second row", "Bazz"))
    )

  el "h2" $ text "Datagrid"
  datagrid 2 $ do
    selectAllEv <- el "thead" $ do
      (selectAllEv, _) <- headMultiSelect $ do
        el "th" $ text ""
        columnHead $ do
          _ <- sortlabel "User ID"
          filterEl BottomRight (constDyn True) blank
        el "th" $ text "Name"
        el "th" $ text "Creation date"
        el "th" $ text "Tickets"
      el "tr" $ do
        el "td" blank
        el "td" blank
        _ <- el "td" $ withFilterCondition $ textInput (inputConfig "")
        _ <- el "td" $ withFilterCondition $ textInput (inputConfig "")
        _ <- el "td" $ withFilterCondition $ textInput (inputConfig "")
        _ <- el "td" $ withFilterCondition $ textInput (inputConfig "")
        pure selectAllEv
    selcountDyn <- elAttr "tbody" ("style" =: "height:20rem") $ do
      (s1, _) <- rowMultiSelect False selectAllEv $ do
        linkCell "#" angleDoubleRightIcon
        el "td" $ text "42"
        _ <- el "td" $ textInput (inputConfig "John Doe")
        _ <- el "td" $ dateInput (inputConfig (Just (fromGregorian 1970 01 01)))
        _ <- el "td" $ numberInput (inputConfig (10 :: Double))
        pure ()
      (s2, _) <- rowMultiSelect False selectAllEv $ do
        linkCell "#" angleDoubleRightIcon
        el "td" $ text "104"
        el "td" $ text "Adam Smith"
        el "td" $ text "Jul 1, 2007"
        elClass "td" "right" $ text "0"
      (s3, _) <- rowMultiSelect False selectAllEv $ do
        linkCell "#" angleDoubleRightIcon
        el "td" $ text "131"
        el "td" $ text "Dimitri Johnson"
        el "td" $ text "May 4, 2014"
        elClass "td" "right" $ text "10"
      let selcountDyn =
            length . filter id <$> mconcat (fmap (fmap (: [])) [s1, s2, s3])
      pure selcountDyn
    el "tfoot" $ do
      el "tr" $ do
        el "td" blank
        el "td" blank
        el "td" $ text "3"
        el "td" blank
        el "td" blank
        elClass "td" "right" $ text "11"
      _ <- tfooter $ do
        selectedCountInfo selcountDyn
        _ <- showHideColumns $ Map.fromList
          [ (1 :: Int, "User ID")
          , (2       , "Name")
          , (3       , "Creation date")
          , (4       , "Tickets")
          ]
        paginationInput (constDyn (Just 51)) def never
      pure ()


  pure never

containerTimeline :: WidgetConstraint js t m => m (Event t URI)
containerTimeline = do
  el "h1" $ text "Timeline"
  timeline timelineSteps
  timelineVertical timelineSteps

  el "h2" $ text "Example"
  el "form" $ timelineVertical $ do
    _ <- timelineStep
      (constDyn "")
      (constDyn TimelineCurrent)
      ""
      (textAreaInput (inputConfig "") >> btnSecondary (text "Comment"))
    _ <- timelineStep
      "2021-06-05\n22:19"
      (constDyn TimelineNotStarted)
      "Adam Smith: "
      (el
        "p"
        (text $ mconcat
          ["Yes, I agree, i think this is better than a simple memo field."]
        )
      )
    _ <- timelineStep
      "2021-06-05\n21:19"
      (constDyn TimelineNotStarted)
      "John Doe: "
      (  el
          "p"
          (text $ mconcat
            [ "The timeline can also be used to build "
            , "a comments component that can be put on just about any page."
            ]
          )
      >> btnSecondary (icon def pencilIcon >> el "span" (text "Edit"))
      )
    pure ()

  pure never

 where
  btnSecondary  = btn def { _buttonConfig_priority = ButtonSecondary }
  timelineSteps = do
    _ <- timelineStep
      (constDyn "21:13 am")
      (constDyn TimelineSuccess)
      "Buy ingredients"
      (  el "p" (text "At the local supermarket perhaps")
      >> btnSecondary (text "Action")
      )
    timelineStep (constDyn "21:23")
                 (constDyn TimelineCurrent)
                 "Mix ingredients"
                 (pure ())
    timelineStep (constDyn "21:43")
                 (constDyn TimelineError)
                 "Put it in the oven"
                 (pure ())
    timelineStep (constDyn "22:10")
                 (constDyn TimelineNotStarted)
                 "Enjoy your cake"
                 (pure ())

containerTreeview
  :: (PostBuild t m, DomBuilder t m, MonadIO m) => m (Event t URI)
containerTreeview = do
  el "h1" $ text "Treeview"
  treeview never (item "ferp-hs") $ do
    treeview never (item "Components") $ do

      treeview never (item "Input") $ do
        leafitem "Basic"
        leafitem "Combobox"
        leafitem "Numeric"

      leafitem "Accordion"
      leafitem "Alert"
      leafitem "Card"
      leafitem "Class"
      leafitem "Icon"
      leafitem "Input"
      leafitem "Navigation"
      leafitem "Progress"
      leafitem "Table"
      leafitem "Tag"
      leafitem "Timeline"

    treeview never (item "Frontend") $ do
      leafitem "Container"
      leafitem "Core"
      leafitem "Input"

    leafitem "DevelMain"
    leafitem "Nordtheme"

  pure never

 where
  item lbl = icon def folderIcon >> text lbl
  leafitem lbl = leaf $ icon def fileIcon >> text lbl
