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
import           Data.Time                      ( Day )
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
import           Servant.Crud.OrderBy           ( fromHasField
                                                , orderByPath
                                                )
import           Servant.Crud.QueryObject       ( QObj )
import           Servant.Crud.QueryOperator     ( MaybeLast(..) )
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
                              (constDyn False)
                              (leftmost [postBuildEv, () <$ updated dynView])

      let deleteBlogReq ev = do
            ev' <- deleteConfirmation (tagPromptlyDyn selection ev)
            pure $ fmap (deleteBlogs usingCookie) ev'

      mDeleteEvResult <- requestBtn deleteBtn
                                    deleteBlogReq
                                    (Set.null <$> selection)
                                    (constDyn False)
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
        { _gridConfig_columns       =
          [ gridProp noEditor   eqFilter  blogIdProp
          , gridProp textEditor strFilter blogTitleProp
          , gridProp textEditor strFilter blogDescriptionProp
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
  rec let postBlogReq ev = attachPromptlyDynWithMaybe
            (\x () -> fmap (postBlog usingCookie) x)
            dynBlog
            ev
      saveResult <- requestBtn saveBtn
                               (pure . postBlogReq)
                               (constDyn False)
                               ((== Nothing) <$> dynBlog)
                               never

      uniqDynBlog <- holdUniqDyn mDynBlog
      modBlogEv   <- undoRedo mempty (updated uniqDynBlog)
      saveEv      <- orAlert saveResult

      mDynBlog    <-
        el "form"
        $    getCompose
        $    pure (mempty { _blogId = pure 0 })
        <**> editWith textEditor       blogTitleProp       mempty modBlogEv
        <**> editWith checkboxEditor   blogIsExtraProp     mempty modBlogEv
        <**> editWith toggleEditor     blogIsPublishedProp mempty modBlogEv
        <**> editWith (req dateEditor) blogDateProp        mempty modBlogEv
        <**> editWith markdownEditor   blogDescriptionProp mempty modBlogEv

      let dynBlog = joinPatch <$> mDynBlog

  pure (fmapMaybe readLocationHeader saveEv)
  where req = requiredEditor

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

  lnks <- widgetHold (pure never) $ ffor initEv $ \initBlog' -> do
    let initBlog = purePatch initBlog'
    backBtn "Close"

    getResp <- requestBtn refreshBtn
                          (pure . (getBlog bid <$))
                          (constDyn False)
                          (constDyn False)
                          never

    rec
      let patchBlogReq ev = attachPromptlyDynWithMaybe
            (\x () -> fmap (patchBlog usingCookie bid) x)
            dynPatch
            ev

      patchEvResult <- requestBtn saveBtn
                                  (pure . patchBlogReq)
                                  ((Just mempty ==) <$> dynPatch)
                                  ((Nothing ==) <$> dynPatch)
                                  patchEv

      rec let deleteBlogReq ev = do
                ev' <- deleteConfirmation (Set.singleton () <$ ev)
                pure $ deleteBlog usingCookie bid <$ ev'

      deleteEvResult <- requestBtn deleteBtn
                                   deleteBlogReq
                                   (constDyn False)
                                   (constDyn False)
                                   never

      uniqDynBlog       <- holdUniqDyn mDynBlog
      debounceDynBlogEv <- debounce
        1
        (difference (updated uniqDynBlog) getBlogEv)
      undoEv          <- undoRedo initBlog debounceDynBlogEv

      getBlogEvManual <- orAlert getResp
      let getBlogEv = leftmost [getNextEv, getBlogEvManual]
      let modBlogEv = leftmost [purePatch <$> getBlogEv, undoEv]

      _               <- orAlert patchEvResult
      deleteEvSuccess <- orAlert deleteEvResult

      dynBlogRemote   <- holdDyn initBlog' getBlogEv

      mDynBlog        <-
        el "form"
        $    getCompose
        $    pure initBlog
        <**> editWith textEditor       blogTitleProp       initBlog modBlogEv
        <**> editWith checkboxEditor   blogIsExtraProp     initBlog modBlogEv
        <**> editWith toggleEditor     blogIsPublishedProp initBlog modBlogEv
        <**> editWith (req dateEditor) blogDateProp        initBlog modBlogEv
        <**> editWith markdownEditor   blogDescriptionProp initBlog modBlogEv

      let dynBlog  = joinPatch <$> mDynBlog

      let dynPatch = makePatch' <$> dynBlogRemote <*> dynBlog
      patchEv <- debounce
        3
        (() <$ fmapMaybe validAndNonempty (updated dynPatch))

    pure $ coerceUri (linkURI (blogsLink mempty)) <$ leftmost
      [() <$ deleteEvSuccess]
  pure (switchDyn lnks)
 where
  makePatch' x my = fmap (makePatch x) my
  validAndNonempty (Just x) | x == mempty = Nothing
                            | otherwise   = Just x
  validAndNonempty Nothing = Nothing

  req = requiredEditor


prop
  :: ( KnownSymbol s
     , HasField s (a (Api.OrderByScope Api.Be)) (C (Api.OrderByScope Api.Be) b)
     , Typeable (a (Api.OrderByScope Api.Be))
     )
  => Text
  -> (forall f . Lens' (a f) (C f b))
  -> Proxy s
  -> Property a b
prop lbl l p =
  let fn = fromHasField p . toApiDirection
  in  Property { _prop_label   = lbl
               , _prop_lens    = l
               , _prop_key     = orderByPath (fn Ascending)
               , _prop_orderBy = fn
               }

editWith
  :: (DomBuilder t m, PostBuild t m, MonadIO m, MonadFix m, Functor c)
  => Editor c t m (Maybe b)
  -> Property a b
  -> a MaybeLast
  -> Event t (a MaybeLast)
  -> Compose m (Dynamic t) (a MaybeLast -> a MaybeLast)
editWith e' prp = formProp e (_prop_label prp) (_prop_lens prp)
  where e = coerceEditor MaybeLast unMaybeLast e'

blogIdProp :: Property BlogT Int64
blogIdProp = prop "Id" blogId (Proxy :: Proxy "_blogId")

blogTitleProp :: Property BlogT Text
blogTitleProp = prop "Title" blogName (Proxy :: Proxy "_blogName")

blogIsExtraProp :: Property BlogT Bool
blogIsExtraProp = prop "Extra" blogIsExtra (Proxy :: Proxy "_blogIsExtra")

blogIsPublishedProp :: Property BlogT Bool
blogIsPublishedProp =
  prop "Published" blogIsPublished (Proxy :: Proxy "_blogIsPublished")

blogDescriptionProp :: Property BlogT Text
blogDescriptionProp =
  prop "Description" blogDescription (Proxy :: Proxy "_blogDescription")

blogDateProp :: Property BlogT Day
blogDateProp = prop "Date" blogDate (Proxy :: Proxy "_blogDate")
