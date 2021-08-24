{-# LANGUAGE ConstraintKinds #-}
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
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Time                      ( fromGregorian )
import           Data.Typeable                  ( Typeable )
import           GHC.Int                        ( Int64 )
import           GHC.Records                    ( HasField(..) )
import           GHC.TypeLits                   ( KnownSymbol )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Crud.API               ( View'(..) )
import           Servant.Crud.OrderBy           ( OrderBy
                                                , Path
                                                , fromHasField
                                                , orderByPath
                                                )
import           Servant.Crud.QueryObject       ( QObj )
import           Servant.Crud.QueryOperator     ( Filter )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router
import           Servant.Subscriber.Reflex
import           URI.ByteString                 ( URI )

import qualified Common.Api                    as Api
import           Common.Auth
import           Common.Schema
import           Components
import           Frontend.Api
import           Frontend.Crud.Datagrid
import           Frontend.Crud.Utils
import           Reflex.Dom.Ace                 ( AceConfig )


-- brittany-disable-next-binding
type CrudApi = Auth Everyone :> "blogs" :> "all" :> QObj (Api.View Api.Be BlogT) :> View
 :<|> Auth Everyone :> "blogs" :> "new" :> View
 :<|> Auth Everyone :> "blogs" :> Capture "key" BlogId :> View

crudApi :: Proxy CrudApi
crudApi = Proxy

blogsLink :: Api.View Api.Be BlogT -> Link
newBlogLink :: Link
blogLink :: BlogId -> Link
blogsLink :<|> newBlogLink :<|> blogLink = allLinks crudApi

crudLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
crudLinks dynUri = safelinkGroup
  (text "Crud")
  [safelink dynUri (blogsLink mempty) $ text "Blogs"]

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
  => Api.View Api.Be BlogT
  -> ApiWidget t m (Event t URI)
blogsHandler vw = do
  el "h1" $ text "Blogs"

  postBuildEv <- getPostBuild

  insertEv    <- insertBtn (constDyn newBlogLink)
  rec let getBlogReq = fmap getBlogs . tagPromptlyDyn dynView
      getBlogEv <- requestBtn refreshBtn
                              (pure . getBlogReq)
                              (constDyn False)
                              (leftmost [postBuildEv, () <$ updated dynView])

      let deleteBlogReq ev = do
            ev' <- deleteConfirmation (tagPromptlyDyn selection ev)
            pure $ fmap (deleteBlogs usingCookie) ev'

      mDeleteEvResult <- requestBtn deleteBtn
                                    deleteBlogReq
                                    (Set.null <$> selection)
                                    never

      downloadButton (getBlogsApiLink <$> dynView)

      deleteEvResult <- orAlert mDeleteEvResult

      recEv          <- orAlert getBlogEv

      keysSet        <- foldDyn ($) def $ leftmost
        [ const . fmapMaybe (fmap primaryKey) . _ms_data <$> updateRows
        , (\d m -> m `Map.withoutKeys` Map.keysSet d) . _ms_data <$> deleteRows
        ]
      let updateRows = attachWith addDeletes
                                  (Map.keysSet <$> current keysSet)
                                  (getListToMapsubset <$> recEv)

      let deleteRows = attachWith
            doDeletes
            ((,) <$> current keysSet <*> current
              (_ms_totalCount <$> _grid_value gridResult)
            )
            deleteEvResult

      gridResult <- datagridDyn DatagridConfig
        { _gridConfig_columns       = [ gridProp blogIdProp
                                      , gridProp blogTitleProp
                                      , gridProp blogDescriptionPropTextbox
                                      ]
        , _gridConfig_selectAll     = False <$ deleteEvResult
        , _gridConfig_setValue      = leftmost [updateRows, deleteRows]
        , _gridConfig_toLink        = blogLink . primaryKey
        , _gridConfig_initialPage   = fromApiPage $ page vw
        , _gridConfig_setPage       = never
        , _gridConfig_toPrimary     = primaryKey
        , _gridConfig_initialSort   = fromApiOrdering $ ordering vw
        , _gridConfig_initialFilter = filters vw
        }

      let selection = _grid_selection gridResult
      dynPage <- holdUniqDyn $ toApiPage <$> _grid_page gridResult
      dynSort <- holdUniqDyn $ _grid_columns gridResult
      let dynView = View <$> dynPage <*> dynSort <*> constDyn mempty
      replaceLocation (blogsLink <$> updated dynView)

  pure (leftmost [_grid_navigate gridResult, insertEv])

 where
  doDeletes (m, c) xs =
    let xsSet = Set.fromList xs
    in  MapSubset
          { _ms_data       = Nothing <$ Map.filter (`Set.member` xsSet) m
          , _ms_totalCount = fmap (\x -> x - fromIntegral (Set.size xsSet)) c
          }


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

  backBtn "Cancel"
  rec let postBlogReq ev =
            attachPromptlyDynWith (\x () -> postBlog usingCookie x) dynBlog ev
      saveResult <- requestBtn saveBtn
                               (pure . postBlogReq)
                               (constDyn False)
                               never

      uniqDynBlog <- holdUniqDyn dynBlog
      modBlogEv   <- undoRedo initBlog (updated uniqDynBlog)
      saveEv      <- orAlert saveResult

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
        -- TODO write the functions
        -- pure :: Blog -> BlogPatch
        -- join :: BlogPatch -> Maybe Blog
        -- Then blog can be inserted / saved if the result of join isJust

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
    backBtn "Close"

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
                ev' <- deleteConfirmation (Set.singleton () <$ ev)
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

    pure $ coerceUri (linkURI (blogsLink mempty)) <$ leftmost
      [() <$ deleteEvSuccess]
  pure (switchDyn lnks)

type Prop a c t m b
  = Property (a Identity) c (Api.ViewOrderBy Api.Be a) (a Filter) Path t m b

propOrderBy
  :: (KnownSymbol s, HasField s r a, c a, Typeable r)
  => Proxy s
  -> (Path, SortOrder -> OrderBy c r)
propOrderBy p =
  let fn = fromHasField p . toApiDirection in (orderByPath (fn Ascending), fn)

filterWith
  :: Lens' r (Filter b)
  -> IndexLens FilterCondition (Filter b) b
  -> IndexLens FilterCondition r b
filterWith l il = il { _ilens_get = _ilens_get il . view l
                     , _ilens_set = \c b -> over l (_ilens_set il c b)
                     }

blogIdProp
  :: (DomBuilder t m, PostBuild t m, MonadHold t m)
  => Prop BlogT (Const ()) t m Int64
blogIdProp = Property { _prop_editor      = noInput
                      , _prop_viewer      = Reflex.Dom.display
                      , _prop_extraConfig = def
                      , _prop_label       = "Id"
                      , _prop_lens        = blogId
                      , _prop_orderBy = propOrderBy (Proxy :: Proxy "_blogId")
                      , _prop_filterBy    = blogId `filterWith` eqFilter
                      }

blogTitleProp
  :: (DomBuilder t m, PostBuild t m) => Prop BlogT (Const ()) t m Text
blogTitleProp = Property
  { _prop_editor      = textInput
  , _prop_viewer      = dynText
  , _prop_extraConfig = def
  , _prop_label       = "Title"
  , _prop_lens        = blogName
  , _prop_orderBy     = propOrderBy (Proxy :: Proxy "_blogName")
  , _prop_filterBy    = blogName `filterWith` strFilter
  }

blogIsExtraProp
  :: (DomBuilder t m, PostBuild t m, MonadIO m)
  => Prop BlogT (Const ()) t m Bool
blogIsExtraProp = Property
  { _prop_editor      = checkboxInput ""
  , _prop_viewer      = Reflex.Dom.display
  , _prop_extraConfig = def
  , _prop_label       = "Extra"
  , _prop_lens        = blogIsExtra
  , _prop_orderBy     = propOrderBy (Proxy :: Proxy "_blogIsExtra")
  , _prop_filterBy    = blogIsExtra `filterWith` eqFilter
  }

blogIsPublishedProp
  :: (DomBuilder t m, PostBuild t m, MonadIO m)
  => Prop BlogT (Const ()) t m Bool
blogIsPublishedProp = Property
  { _prop_editor      = toggleInput ""
  , _prop_viewer      = Reflex.Dom.display
  , _prop_extraConfig = def
  , _prop_label       = "Published"
  , _prop_lens        = blogIsPublished
  , _prop_orderBy     = propOrderBy (Proxy :: Proxy "_blogIsPublished")
  , _prop_filterBy    = blogIsPublished `filterWith` eqFilter
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
  => Prop BlogT (Const AceConfig) t m Text
blogDescriptionProp = Property
  { _prop_editor      = markdownInput
  , _prop_viewer      = dynText
  , _prop_extraConfig = cdnAceConfig
  , _prop_label       = "Description"
  , _prop_lens        = blogDescription
  , _prop_orderBy     = propOrderBy (Proxy :: Proxy "_blogDescription")
  , _prop_filterBy    = blogDescription `filterWith` strFilter
  }

blogDescriptionPropTextbox
  :: (DomBuilder t m, PostBuild t m) => Prop BlogT (Const ()) t m Text
blogDescriptionPropTextbox = Property
  { _prop_editor      = textInput
  , _prop_viewer      = dynText
  , _prop_extraConfig = def
  , _prop_label       = "Description"
  , _prop_lens        = blogDescription
  , _prop_orderBy     = propOrderBy (Proxy :: Proxy "_blogDescription")
  , _prop_filterBy    = blogDescription `filterWith` strFilter
  }

