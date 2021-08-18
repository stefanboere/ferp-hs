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
  ) where

import           Control.Applicative            ( (<**>) )
import           Control.Lens
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
import           Reflex.Dom.Ace                 ( AceConfig )


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
     , MonadIO m
     )
  => ApiWidget t m (Event t URI)
blogsHandler = do
  el "h1" $ text "Blogs"

  postBuildEv <- getPostBuild

  let getBlogEv = getBlogs mempty <$ postBuildEv
  recEv <- orAlertF $ requestingJs getBlogEv

  datagridDyn (blogLink . BlogId)
              [gridProp blogTitleProp, gridProp blogDescriptionPropTextbox]
              Map.empty
              (toMap . getResponse <$> recEv)
  where toMap = Map.fromList . fmap (\b -> (_blogId b, Just b))


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
  let getRespEv = getBlog bid <$ postBuildEv
  (initEv, getNextEv) <- orAlertF (requestingJs getRespEv) >>= headTailE

  widgetHold_ (pure ()) $ ffor initEv $ \initBlog -> do

    getResp <- requestBtn refreshBtn (getBlog bid <$) (constDyn False) never

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
        <**> formProp blogTitleProp       initBlog modBlogEv
        <**> formProp blogIsExtraProp     initBlog modBlogEv
        <**> formProp blogIsPublishedProp initBlog modBlogEv
        <**> formProp blogDescriptionProp initBlog modBlogEv

      let dynPatch = makePatch <$> dynBlogRemote <*> dynBlog
      patchEv <- throttle 10 (() <$ ffilter (/= mempty) (updated dynPatch))
    pure ()

  pure never

blogTitleProp
  :: (DomBuilder t m, PostBuild t m, MonadFix m)
  => Property Blog (Const ()) t m Text
blogTitleProp = Property { _prop_editor      = textInput
                         , _prop_extraConfig = def
                         , _prop_label       = "Title"
                         , _prop_lens        = blogName
                         }

blogIsExtraProp
  :: (DomBuilder t m, PostBuild t m, MonadIO m)
  => Property Blog (Const ()) t m Bool
blogIsExtraProp = Property { _prop_editor      = checkboxInput ""
                           , _prop_extraConfig = def
                           , _prop_label       = "Extra"
                           , _prop_lens        = blogIsExtra
                           }

blogIsPublishedProp
  :: (DomBuilder t m, PostBuild t m, MonadIO m)
  => Property Blog (Const ()) t m Bool
blogIsPublishedProp = Property { _prop_editor      = toggleInput ""
                               , _prop_extraConfig = def
                               , _prop_label       = "Published"
                               , _prop_lens        = blogIsPublished
                               }

blogDescriptionProp
  :: ( DomBuilder t m
     , PostBuild t m
     , TriggerEvent t m
     , MonadHold t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , MonadFix m
     , Prerender js t m
     )
  => Property Blog (Const AceConfig) t m Text
blogDescriptionProp = Property { _prop_editor      = markdownInput
                               , _prop_extraConfig = cdnAceConfig
                               , _prop_label       = "Description"
                               , _prop_lens        = blogDescription
                               }

blogDescriptionPropTextbox
  :: (DomBuilder t m, PostBuild t m, MonadFix m)
  => Property Blog (Const ()) t m Text
blogDescriptionPropTextbox = Property { _prop_editor      = textInput
                                      , _prop_extraConfig = def
                                      , _prop_label       = "Description"
                                      , _prop_lens        = blogDescription
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
