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
import           Control.Monad                  ( void )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Functor.Compose
import qualified Data.Map                      as Map
import           Data.Proxy
import           Data.Text                      ( Text )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router
import           Servant.Subscriber.Reflex
import           URI.ByteString                 ( URI )

import           Common.Auth
import           Common.Schema
import           Components
import           Frontend.Api
import           Reflex.Markdown


-- brittany-disable-next-binding
type ProtectedApi = Auth Everyone :> "blogs" :> "all" :> View
 :<|> "blogs" :> Capture "key" BlogId :> View

protectedApi :: Proxy ProtectedApi
protectedApi = Proxy

blogsLink :: Link
blogLink :: BlogId -> Link
blogsLink :<|> blogLink = allLinks protectedApi

protectedLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
protectedLinks dynUri = safelinkGroup
  (text "Protected")
  [ safelink dynUri blogsLink $ text "Blogs"
  , safelink dynUri (blogLink (BlogId 1)) $ text "Blog 1"
  ]

protectedHandler
  :: WidgetConstraint js t m => RouteT ProtectedApi m (Event t URI)
protectedHandler = blogsHandler :<|> blogEdit

blogsHandler
  :: forall js t m
   . ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     )
  => m (Event t URI)
blogsHandler = runApi $ do
  el "h1" $ text "Blogs"

  postBuildEv <- getPostBuild

  let getBlogEv = getBlogs mempty <$ postBuildEv
  recEv    <- orAlertF $ requestingJs getBlogEv

  mayOkDyn <- holdDyn Nothing (Just . getResponse <$> recEv)

  loading mayOkDyn $ \okDyn -> do
    tableDyn
      [ (text "Title"      , \_ b -> dynText (_blogName <$> b))
      , (text "Description", \_ b -> dynText (_blogDescription <$> b))
      ]
      (Map.fromList . zip [(0 :: Int) ..] <$> okDyn)


  pure never


blogEdit
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
  => BlogId
  -> m (Event t URI)
blogEdit bid = runApi $ do
  el "h1" $ text "Blog 1"

  postBuildEv <- getPostBuild
  let getRespEv = getBlog usingCookie bid <$ postBuildEv
  initEv <- orAlertF $ requestingJs getRespEv

  widgetHold_ (pure ()) $ ffor initEv $ \initBlog -> do

    getResp <- requestBtn refreshBtn
                          (getBlog usingCookie bid <$)
                          (constDyn False)
                          never

    rec
      let patchBlogReq ev = attachPromptlyDynWith
            (\x () -> patchBlog usingCookie bid x)
            dynPatch
            ev
      patchRespEv <- requestBtn saveBtn
                                patchBlogReq
                                ((== mempty) <$> dynPatch)
                                patchEv

      getBlogEv     <- orAlert getResp

      patchDoneEv   <- orAlert patchRespEv

      dynBlogRemote <- holdDyn
        initBlog
        (leftmost [getBlogEv, tagPromptlyDyn dynBlog patchDoneEv])

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

        <**> prop markdownInput "Description" blogDescription initBlog getBlogEv

      let dynPatch = makePatch <$> dynBlogRemote <*> dynBlog
      patchEv <- throttle 2 (() <$ ffilter (/= mempty) (updated dynPatch))
    pure ()

  pure never


prop
  :: (DomBuilder t m, PostBuild t m, MonadIO m)
  => (InputConfig t b -> m (Dynamic t b))
  -> Text
  -> Lens' a b
  -> a
  -> Event t a
  -> Compose m (Dynamic t) (a -> a)
prop editor lbl l initVal update = Compose $ fmap (set l) <$> labeled
  lbl
  editor
  (inputConfig (view l initVal)) { _inputConfig_setValue = view l <$> update }

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

loading
  :: (MonadFix m, PostBuild t m, DomBuilder t m, MonadHold t m)
  => Dynamic t (Maybe a)
  -> (Dynamic t a -> m b)
  -> m ()
loading dynX w = do
  factored <- maybeDyn dynX

  dyn_ $ maybe (pure ()) (void . w) <$> factored
