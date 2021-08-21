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
import           Data.Proxy
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Time                      ( fromGregorian )
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
 :<|> Auth Everyone :> "blogs" :> "new" :> View
 :<|> Auth Everyone :> "blogs" :> Capture "key" BlogId :> View

crudApi :: Proxy CrudApi
crudApi = Proxy

blogsLink, newBlogLink :: Link
blogLink :: BlogId -> Link
blogsLink :<|> newBlogLink :<|> blogLink = allLinks crudApi

crudLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
crudLinks dynUri =
  safelinkGroup (text "Crud") [safelink dynUri blogsLink $ text "Blogs"]

crudHandler
  :: WidgetConstraint js t m => RouteT CrudApi (ApiWidget t m) (Event t URI)
crudHandler = blogsHandler :<|> newBlogHandler :<|> blogEdit

blogsHandler
  :: forall js t m
   . ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , MonadIO m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     )
  => ApiWidget t m (Event t URI)
blogsHandler = do
  el "h1" $ text "Blogs"

  postBuildEv <- getPostBuild

  insertEv    <- insertBtn
  rec
    let deleteBlogReq ev = do
          ev' <- deleteConfirmation
            (tagPromptlyDyn (_grid_selection gridResult) ev)
          pure $ fmap (deleteBlogs usingCookie . fmap primaryKey) ev'

    getBlogEv <- requestBtn refreshBtn
                            (pure . (getBlogs mempty <$))
                            (constDyn False)
                            postBuildEv

    deleteEvResult <-
      requestBtn deleteBtn
                 deleteBlogReq
                 (null <$> _grid_selection gridResult)
                 never
        >>= orAlert

    recEv      <- orAlert getBlogEv

    gridResult <- datagridDyn
      (blogLink . primaryKey)
      [gridProp blogTitleProp, gridProp blogDescriptionPropTextbox]
      (False <$ deleteEvResult)
      (getListToMapsubset <$> recEv)

  pure
    (leftmost
      [_grid_navigate gridResult, coerceUri (linkURI newBlogLink) <$ insertEv]
    )

newBlogHandler
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
  => ApiWidget t m (Event t URI)
newBlogHandler = do
  el "h1" $ text "New blog"

  rec let postBlogReq ev =
            attachPromptlyDynWith (\x () -> postBlog usingCookie x) dynBlog ev
      saveEv <-
        requestBtn saveBtn (pure . postBlogReq) (constDyn False) never
          >>= orAlert

      uniqDynBlog <- holdUniqDyn dynBlog
      modBlogEv   <- undoRedo initBlog (updated uniqDynBlog)

      dynBlog     <-
        el "form"
        $    getCompose
        $    pure initBlog
        <**> formProp blogTitleProp       initBlog modBlogEv
        <**> formProp blogIsExtraProp     initBlog modBlogEv
        <**> formProp blogIsPublishedProp initBlog modBlogEv
        <**> formProp blogDescriptionProp initBlog modBlogEv

  pure (fmapMaybe readLocationHeader saveEv)
  where initBlog = Blog 0 mempty mempty False False (fromGregorian 2021 08 19)

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
blogEdit bid@(BlogId blogIdNum) = do
  el "h1" $ text ("Blog " <> pack (show blogIdNum))

  postBuildEv <- getPostBuild
  let getRespEv = getBlog bid <$ postBuildEv
  (initEv, getNextEv) <- orAlertF (requestingJs getRespEv) >>= headTailE

  lnks                <- widgetHold (pure never) $ ffor initEv $ \initBlog -> do
    backEv  <- backBtn

    getResp <- requestBtn refreshBtn
                          (pure . (getBlog bid <$))
                          (constDyn False)
                          never

    rec
      let patchBlogReq ev = attachPromptlyDynWith
            (\x () -> patchBlog usingCookie bid x)
            dynPatch
            ev

      patchEvResult <- requestBtn saveBtn
                                  (pure . patchBlogReq)
                                  ((== mempty) <$> dynPatch)
                                  patchEv

      rec let deleteBlogReq ev = do
                ev' <- deleteConfirmation ([()] <$ ev)
                pure $ deleteBlog usingCookie bid <$ ev'

      deleteEvResult <- requestBtn deleteBtn
                                   deleteBlogReq
                                   (constDyn False)
                                   never

      uniqDynBlog       <- holdUniqDyn dynBlog
      debounceDynBlogEv <- debounce
        1
        (difference (updated uniqDynBlog) getBlogEv)
      undoEv          <- undoRedo initBlog debounceDynBlogEv

      getBlogEvManual <- orAlert getResp
      let getBlogEv = leftmost [getNextEv, getBlogEvManual]
      let modBlogEv = leftmost [getBlogEv, undoEv]

      _               <- orAlert patchEvResult
      deleteEvSuccess <- orAlert deleteEvResult

      dynBlogRemote   <- holdDyn initBlog getBlogEv

      dynBlog         <-
        el "form"
        $    getCompose
        $    pure initBlog
        <**> formProp blogTitleProp       initBlog modBlogEv
        <**> formProp blogIsExtraProp     initBlog modBlogEv
        <**> formProp blogIsPublishedProp initBlog modBlogEv
        <**> formProp blogDescriptionProp initBlog modBlogEv

      let dynPatch = makePatch <$> dynBlogRemote <*> dynBlog
      patchEv <- throttle 10 (() <$ ffilter (/= mempty) (updated dynPatch))

    pure $ coerceUri (linkURI blogsLink) <$ leftmost
      [() <$ deleteEvSuccess, backEv]
  pure (switchDyn lnks)

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
  btn def { _buttonConfig_state    = stateDyn
          , _buttonConfig_priority = ButtonSecondary
          }
    $  icon def ico
    >> el "span" (dynText (stateText <$> stateDyn))
 where
  stateText ActionAvailable = x
  stateText ActionError     = x
  stateText ActionLoading   = xing
  stateText ActionSuccess   = xed
  stateText ActionDisabled  = xed

backBtn :: (PostBuild t m, DomBuilder t m) => m (Event t ())
backBtn =
  btn def { _buttonConfig_priority = ButtonSecondary }
    $  icon def timesIcon
    >> el "span" (text "Close")

saveBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
saveBtn = triStateBtn floppyIcon ("Save", "Saving", "Saved")

refreshBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
refreshBtn = triStateBtn refreshIcon ("Reload", "Reloading", "Reloaded")

insertBtn :: (PostBuild t m, DomBuilder t m) => m (Event t ())
insertBtn =
  btn def { _buttonConfig_priority = ButtonSecondary } $ icon def plusIcon >> el
    "span"
    (text "Insert")

deleteBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
deleteBtn = triStateBtn trashIcon ("Delete", "Deleting", "Delete")

deleteConfirmation
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => Event t [a]
  -> m (Event t [a])
deleteConfirmation = messageBox
  "Confirm deletion"
  (getMsg . length)
  (btn def { _buttonConfig_priority = ButtonPrimary Danger } (text "Delete"))

 where
  getMsg 1 = "Are you sure you want to delete this item?"
  getMsg l =
    "Are you sure you want to delete these " <> pack (show l) <> " items?"

messageBox
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => Text
  -> (a -> Text)
  -> m (Event t ())
  -> Event t a
  -> m (Event t a)
messageBox titl msg okBtn openEv = fmapMaybe id
  <$> modal ModalMedium (modalContent <$> openEv)
 where
  modalContent y = card $ do
    x <- cardHeader (text titl >> modalCloseBtn)

    cardContent $ el "p" $ text (msg y)

    cardFooter $ do
      cancelEv <- cardAction "Cancel"
      okEv     <- okBtn
      pure $ leftmost [Nothing <$ x, Nothing <$ cancelEv, Just y <$ okEv]
