{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Revision
  ( revisionHandler
  , revisionLinks
  , RevisionApi
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Data.Proxy
import qualified Data.Map.Strict               as Map
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
type RevisionApi = "revision" :> "transaction" :> View

revisionApi :: Proxy RevisionApi
revisionApi = Proxy

transactionLink :: Link
transactionLink = allLinks revisionApi

revisionLinks
  :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
revisionLinks dynUri = safelinkGroup
  (text "Revisions")
  [safelink dynUri transactionLink $ text "Transactions"]

revisionHandler :: WidgetConstraint js t m => RouteT RevisionApi m (Event t URI)
revisionHandler = transactionHandler

transactionHandler :: WidgetConstraint js t m => m (Event t URI)
transactionHandler = do
  el "h1" $ text "Transactions"

  tabs $ Map.fromList
    [ (0 :: Integer, ("Commits", commits))
    , (1           , ("Branches", branches))
    , (2           , ("Releases", releases))
    ]

  pure never
 where
  btnSecondary = btn def { _buttonConfig_priority = ButtonSecondary }

  branchesList = Map.fromList
    $ zip [(1 :: Integer), 2 ..] ["master", "develop", "feature/feature1"]
  showOpt _ = dynText

  branch = labeled "Branch"
                   (comboboxInput showOpt branchesList)
                   (inputConfig "branch" def)

  branches = do
    el "table" $ do
      el "thead" $ do
        el "tr" $ do
          elAttr "th" (Map.singleton "colspan" "4") $ text "Branches"
      el "tbody" $ do
        el "tr" $ do
          linkCell "#" angleDoubleRightIcon
          el "td" $ text "Master"
          _ <- el "td" $ tagEl def "Default"
          _ <- el "td" $ btnSecondary (icon def plusIcon >> text "New branch")
          pure ()
        el "tr" $ do
          linkCell "#" angleDoubleRightIcon
          el "td" $ text "Develop"
          el "td" blank
          el "td" branchActions
        el "tr" $ do
          linkCell "#" angleDoubleRightIcon
          el "td" $ text "Feature/feature1"
          el "td" blank
          el "td" branchActions

  branchActions = do
    _ <- btnSecondary (icon def plusIcon >> text "New branch") -- TODO change for forking
    _ <- btnSecondary (icon def pencilIcon >> text "Merge") -- TODO maybe change for install or a custom merge icon
    _ <- btnSecondary (icon def trashIcon >> text "Delete")
    pure ()

  commits = do
    el "form" $ do
      _ <- branch
      _ <- labeled "Hide other transactions"
                   (toggleInput "")
                   (inputConfig "cmt1" False)
      pure ()

    timelineVertical $ do
      _ <- timelineStep
        (constDyn "")
        (constDyn TimelineCurrent)
        ""
        (  textAreaInput (inputConfig "tltb2" "")
        >> btnSecondary (text "Create new commit")
        >> btnSecondary (icon def plusIcon >> text "New branch") -- TODO change for forking
        )
      _ <- timelineStep "2021-06-05\n22:24"
                        (constDyn TimelineNotStarted)
                        "Adam Smith"
                        blank
      _ <- timelineStep "2021-06-05\n22:22"
                        (constDyn TimelineNotStarted)
                        "Adam Smith"
                        blank
      _ <- timelineStep "2021-06-05\n22:19"
                        (constDyn TimelineSuccess)
                        "Adam Smith"
                        (tagEl def "beta" >> el "p" (text "Bugfixes"))
      _ <- timelineStep
        "2021-06-05\n21:19"
        (constDyn TimelineSuccess)
        "John Doe"
        (tagEl def "alpha" >> el
          "p"
          (text $ mconcat
            [ "The timeline can also be used to build "
            , "a comments component that can be put on just about any page."
            ]
          )
        )
      _ <- timelineStep "2021-06-05\n21:00"
                        (constDyn TimelineSuccess)
                        "John Doe"
                        (el "p" (text "Initial commit"))
      pure ()

  releases = timelineVertical $ do
    _ <- timelineStep
      (constDyn "")
      (constDyn TimelineCurrent)
      ""
      (do
        _ <- labeled "Tag name" textInput (inputConfig "rls0" "")
        _ <- branch
        _ <- labeled "Title" textInput (inputConfig "rls2" "")
        _ <- labeled "Content" textAreaInput (inputConfig "rls3" "")
        _ <- toggleInput "Stable release" (inputConfig "rls4" True)
        btnSecondary (text "Create new release")
      )
    _ <- timelineStep "Beta \n@ master"
                      (constDyn TimelineSuccess)
                      "Beta release (stable)"
                      (el "p" $ text "2021-06-05\n22:25")
    _ <- timelineStep "Alpha \n@ master"
                      (constDyn TimelineNotStarted)
                      "Alpha release"
                      (el "p" $ text "2021-06-05\n21:25")
    pure ()

