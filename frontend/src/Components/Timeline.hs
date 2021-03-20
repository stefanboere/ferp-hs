{-# LANGUAGE OverloadedStrings #-}
module Components.Timeline
  ( timelineStyle
  , timeline
  , timelineVertical
  , timelineStep
  , TimelineState(..)
  )
where

import           Prelude                 hiding ( rem
                                                , (**)
                                                )

import           Clay                    hiding ( icon )
import qualified Clay.Media                    as Media
import           Data.Default
import           Data.Text                      ( Text )
import           Reflex.Dom              hiding ( display
                                                , (&)
                                                )

import           Components.Class
import           Components.Icon
import           Components.Progress
import           Nordtheme

timelineStyle :: Css
timelineStyle = do
  timelineVerticalStyle
  query Clay.all [Media.minWidth 768] timelineHorizontalStyle
  ".timeline" ? do
    paddingAll (rem 1)
    display grid
    "grid-column-gap" -: "1rem"
    "grid-row-gap" -: "1rem"
    "fill" -: showColor (lighten 0.5 grey0')

  ".timeline-header" ? do
    fontSize (rem (13 / 16))
    fontColor grey0'
    fontWeight (weight 400)

  ".timeline-icon" ? do
    height (rem 2)

  ".timeline-body" ? do
    position relative
    fontSize (rem (11 / 16))

    before & do
      content (stringContent "")
      display block
      border solid 1 (lighten 0.5 grey0')
      backgroundColor (lighten 0.5 grey0')

    lastChild & before & display none

timelineHorizontalStyle :: Css
timelineHorizontalStyle = do
  ".timeline-horizontal" ? do
    "grid-template-columns" -: "repeat(5, minmax(10rem, 1fr))"
    "grid-template-rows" -: "1rem 2rem auto"

    ".timeline-header" ? do
      "grid-row" -: "1"
      "grid-column" -: "auto"
      marginTop nil

    ".timeline-icon" ? do
      "grid-row" -: "2"
      "grid-column" -: "auto"

    ".timeline-body" ? do
      "grid-row" -: "3"
      "grid-column" -: "auto"
      paddingTop nil

      before & do
        position relative
        top (rem (-2))
        left (rem 2)
        width (pct 100 @-@ rem 1)
        height nil

timelineVerticalStyle :: Css
timelineVerticalStyle = do
  ".timeline" ? do
    "grid-template-columns" -: "max-content 2rem 1fr"
    "grid-template-rows" -: "auto"

  ".timeline-header" ? do
    "grid-column" -: "1"
    marginTop (rem (9 / 16))

  ".timeline-icon" ? do
    "grid-column" -: "2"

  ".timeline-body" ? do
    "grid-column" -: "3"
    paddingTop (rem (1 / 2))

    before & do
      position absolute
      left (rem (-2))
      top (rem 2)
      height (pct 100 @-@ rem 1)


timeline :: DomBuilder t m => m a -> m a
timeline = elClass "div" "timeline timeline-horizontal"

timelineVertical :: DomBuilder t m => m a -> m a
timelineVertical = elClass "div" "timeline timeline-vertical"

data TimelineState = TimelineSuccess -- ^ Step complete (success-standard shape)
                   | TimelineCurrent -- ^ Current step (dot-circle shape)
                   | TimelineNotStarted -- ^ Not started (circle shape)
                   | TimelineLoading -- ^ Processing user initiated action (spinner)
                   | TimelineError  -- ^ Error completing step (error-standard shape)

instance Default TimelineState where
  def = TimelineNotStarted

timelineStep
  :: (PostBuild t m, DomBuilder t m)
  => Dynamic t Text
  -> Dynamic t TimelineState
  -> Dynamic t Text
  -> m a
  -> m a
timelineStep hdr state' titl cnt = do
  elClass "div" "timeline-header" $ dynText hdr
  _ <- elClass "div" "timeline-icon" $ dyn (stateEl <$> state')
  elClass "div" "timeline-body" $ do
    elClass "div" "timeline-title p2" $ dynText titl
    cnt
 where
  stateEl TimelineSuccess = icon
    defIcon { _iconConfig_status = constDyn $ Just Success }
    successStandardIcon
  stateEl TimelineCurrent =
    icon defIcon { _iconConfig_status = constDyn $ Just Info } dotCircleIcon
  stateEl TimelineNotStarted = icon defIcon circleIcon
  stateEl TimelineLoading    = spinner Medium ""
  stateEl TimelineError      = icon
    defIcon { _iconConfig_status = constDyn $ Just Danger }
    errorStandardIcon

  defIcon = def { _iconConfig_size = 2 }
