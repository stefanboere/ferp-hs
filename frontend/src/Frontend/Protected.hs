{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Protected
  ( protectedHandler
  , protectedLinks
  , ProtectedApi
  )
where

import           Control.Applicative            ( (<**>) )
import           Control.Lens
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Functor.Compose
import           Data.Proxy
import           Data.Text                      ( Text )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import           URI.ByteString                 ( URI )

import           Servant.Router

import           Common.Auth
import           Common.Schema
import           Components
import           Frontend.Api
import           Reflex.Markdown


type ProtectedApi = Auth Everyone :> "user" :> "self" :> View

protectedApi :: Proxy ProtectedApi
protectedApi = Proxy

protectedSelfLink :: Link
protectedSelfLink = allLinks protectedApi

protectedLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
protectedLinks dynUri = safelinkGroup
  (text "Protected")
  [safelink dynUri protectedSelfLink $ text "User"]

protectedHandler
  :: WidgetConstraint js t m => RouteT ProtectedApi m (Event t URI)
protectedHandler = protectedSelf

protectedSelf
  :: forall js t m
   . ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , MonadIO m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , PerformEvent t m
     )
  => m (Event t URI)
protectedSelf = do
  el "h1" $ text "Current User"

  postBuildEv <- getPostBuild
  let getRespEv = getBlog (constDyn . pure $ BlogId 1)
  initEv <- orAlertBg $ getRespEv postBuildEv

  widgetHold_ (spinner Large "Loading") $ ffor initEv $ \initBlog -> do

    getResp <- requestBtn refreshBtn getRespEv (constDyn False) never

    rec
      let patchBlogReq ev = patchBlog (constDyn . pure $ BlogId 1)
                                      (pure <$> dynPatch)
                                      (tagPromptlyDyn dynBlog ev)
      patchRespEv <- requestBtn saveBtn
                                patchBlogReq
                                ((== mempty) <$> dynPatch)
                                patchEv

      getBlogEv     <- orAlert getResp

      patchDoneEv   <- orAlert' patchRespEv

      dynBlogRemote <- holdDyn initBlog
                               (leftmost [getBlogEv, fst <$> patchDoneEv])

      dynBlog <-
        el "form"
        $    getCompose
        $    pure initBlog
        <**> textProp "Title" blogName initBlog getBlogEv

        <**> prop (checkboxInput "") "Extra" blogIsExtra initBlog getBlogEv

        <**> prop (toggleInput "")
                  "Published"
                  blogIsPublished
                  initBlog
                  getBlogEv

        <**> prop (markdownInput mempty)
                  "Description"
                  blogDescription
                  initBlog
                  getBlogEv

      let dynPatch = makePatch <$> dynBlogRemote <*> dynBlog
      patchEv <- debounce 2 (() <$ ffilter (/= mempty) (updated dynPatch))
    pure ()

  pure never


prop
  :: (Functor m, Reflex t)
  => (InputConfig t b -> m (Dynamic t b))
  -> Text
  -> Lens' a b
  -> a
  -> Event t a
  -> Compose m (Dynamic t) (a -> a)
prop editor lbl l initVal update = Compose $ fmap (set l) <$> editor
  (inputConfig (view l initVal)) { _inputConfig_label    = constDyn lbl
                                 , _inputConfig_setValue = view l <$> update
                                 }

textProp
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadIO m)
  => Text
  -> Lens' a Text
  -> a
  -> Event t a
  -> Compose m (Dynamic t) (a -> a)
textProp = prop (fmap _inputEl_value . textInput)

triStateBtn
  :: (PostBuild t m, DomBuilder t m)
  => m ()
  -> (Text, Text, Text)
  -> Dynamic t ActionState
  -> m (Event t ())
triStateBtn ico (x, xing, xed) stateDyn =
  btn def { _buttonConfig_state = stateDyn } $ icon def ico >> el
    "span"
    (dynText (stateText <$> stateDyn))
 where
  stateText ActionAvailable = x
  stateText ActionError     = x
  stateText ActionLoading   = xing
  stateText ActionSuccess   = xed
  stateText ActionDisabled  = xed

saveBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
saveBtn = triStateBtn floppyIcon ("Save", "Saving", "Saved")

refreshBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
refreshBtn = triStateBtn refreshIcon ("Reload", "Reloading", "Reloaded")
