{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Crud
  ( crudHandler
  , crudLinks
  , CrudApi
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


-- brittany-disable-next-binding
type CrudApi = Auth Everyone :> "blogs" :> "all" :> View
 :<|> Auth Everyone :> "blogs" :> Capture "key" BlogId :> View

crudApi :: Proxy CrudApi
crudApi = Proxy

blogsLink :: Link
blogLink :: BlogId -> Link
blogsLink :<|> blogLink = allLinks crudApi

crudLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
crudLinks dynUri = safelinkGroup
  (text "Crud")
  [ safelink dynUri blogsLink $ text "Blogs"
  , safelink dynUri (blogLink (BlogId 1)) $ text "Blog 1"
  ]

crudHandler
  :: WidgetConstraint js t m => RouteT CrudApi (ApiWidget t m) (Event t URI)
crudHandler = blogsHandler :<|> blogEdit

blogsHandler
  :: forall js t m
   . ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     )
  => ApiWidget t m (Event t URI)
blogsHandler = do
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
  -> ApiWidget t m (Event t URI)
blogEdit bid = do
  el "h1" $ text "Blog 1"

  postBuildEv <- getPostBuild
  let getRespEv = getBlog usingCookie bid <$ postBuildEv
  (initEv, getNextEv) <- orAlertF (requestingJs getRespEv) >>= headTailE

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
      _ <-
        requestBtn saveBtn patchBlogReq ((== mempty) <$> dynPatch) patchEv
          >>= orAlert

      uniqDynBlog       <- holdUniqDyn dynBlog
      debounceDynBlogEv <- debounce
        1
        (difference (updated uniqDynBlog) getBlogEv)
      undoEv          <- undoRedo initBlog debounceDynBlogEv

      getBlogEvManual <- orAlert getResp
      let getBlogEv = leftmost [getNextEv, getBlogEvManual]
      let modBlogEv = leftmost [getBlogEv, undoEv]

      dynBlogRemote <- holdDyn initBlog getBlogEv

      dynBlog       <-
        el "form"
        $    getCompose
        $    pure initBlog
        <**> prop def textInput          "Title" blogName    initBlog modBlogEv

        <**> prop def (checkboxInput "") "Extra" blogIsExtra initBlog modBlogEv

        <**> prop def
                  (toggleInput "")
                  "Published"
                  blogIsPublished
                  initBlog
                  modBlogEv

        <**> prop cdnAceConfig
                  markdownInput
                  "Description"
                  blogDescription
                  initBlog
                  modBlogEv

      let dynPatch = makePatch <$> dynBlogRemote <*> dynBlog
      patchEv <- throttle 10 (() <$ ffilter (/= mempty) (updated dynPatch))
    pure ()

  pure never


-- | Converts an editor into an editor which accounts for focus
--
-- 1. If the user is focussed, no external set value events are applied
-- 2. The output dynamic is only updated on lose focus
respectFocus
  :: (DomBuilder t m, MonadFix m)
  => (InputConfig' c t b -> m (DomInputEl t m b))
  -> InputConfig' c t b
  -> m (DomInputEl t m b)
respectFocus editor cfg = do
  rec r <- editor cfg
        { _inputConfig_setValue = gate (current (not <$> _inputEl_hasFocus r))
                                       (_inputConfig_setValue cfg)
        }

  pure r

prop
  :: (DomBuilder t m, PostBuild t m, MonadIO m, MonadFix m)
  => c b
  -> (InputConfig' c t b -> m (DomInputEl t m b))
  -> Text
  -> Lens' a b
  -> a
  -> Event t a
  -> Compose m (Dynamic t) (a -> a)
prop c editor lbl l initVal update =
  Compose $ fmap (set l) . _inputEl_value <$> labeled
    lbl
    (respectFocus editor)
    (inputConfig' c (view l initVal)) { _inputConfig_setValue = view l
                                        <$> update
                                      }

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
