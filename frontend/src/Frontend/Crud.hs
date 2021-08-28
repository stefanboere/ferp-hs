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
crudHandler = blogsHandler :<|> blogEdit Nothing :<|> (blogEdit . Just)

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
blogsHandler vw = elClass "div" "flex-column" $ do
  el "h1" $ text "Blogs"

  postBuildEv <- getPostBuild

  rec let getBlogReq = fmap getBlogs . tagPromptlyDyn dynView
      (insertEv, getBlogEv, mDeleteEvResult) <- el "div" $ do
        insertEv'  <- insertBtn (constDyn newBlogLink)
        getBlogEv' <- requestBtn
          refreshBtn
          (pure . getBlogReq)
          (constDyn False)
          (constDyn False)
          (leftmost [postBuildEv, () <$ updated dynView])

        let deleteBlogReq ev = do
              ev' <- deleteConfirmation (tagPromptlyDyn selection ev)
              pure $ fmap (deleteBlogs usingCookie) ev'

        mDeleteEvResult' <- requestBtn deleteBtn
                                       deleteBlogReq
                                       (Set.null <$> selection)
                                       (constDyn False)
                                       never

        downloadButton (getBlogsApiLink <$> dynView)
        pure (insertEv', getBlogEv', mDeleteEvResult')

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
        , _gridConfig_initialWindow = winFromApiPage $ page vw
        , _gridConfig_toPrimary     = primaryKey
        , _gridConfig_initialSort   = fromApiOrdering $ ordering vw
        , _gridConfig_initialFilter = filters vw
        }

      let selection = _grid_selection gridResult
      dynPage <- holdUniqDyn $ winToApiPage <$> _grid_window gridResult
      dynSort           <- holdUniqDyn $ _grid_columns gridResult
      dynFilterDebounce <- debounce 1 $ updated (_grid_filter gridResult)
      dynFilter         <- holdDyn (filters vw) dynFilterDebounce
      dynFilterUniq     <- holdUniqDyn dynFilter
      let dynView = View <$> dynPage <*> dynSort <*> dynFilterUniq
      replaceLocation (blogsLink <$> updated dynView)

  pure (leftmost [_grid_navigate gridResult, insertEv])

 where
  doDeletes (m, c) xs =
    let xsSet = Set.fromList xs
    in  MapSubset
          { _ms_data       = Nothing <$ Map.filter (`Set.member` xsSet) m
          , _ms_totalCount = fmap (\x -> x - fromIntegral (Set.size xsSet)) c
          }

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
  => Maybe BlogId
  -> ApiWidget t m (Event t URI)
blogEdit initBlogId = do

  postBuildEv <- getPostBuild

  rec
    let setBlogIdEv = Just . getResponse <$> postEvSuccess
    dynBlogId <- holdDyn initBlogId setBlogIdEv
    el "h1" $ dynText (mkHeader <$> dynBlogId)

    backBtn "Close"

    getResp       <- dynButton initBlogId setBlogIdEv (getButton postBuildEv)

    patchEvResult <- dynButton initBlogId
                               setBlogIdEv
                               (patchButton dynPatch patchEv)
    postEvResult      <- dynButton initBlogId setBlogIdEv (postButton dynBlog)
    deleteEvResult    <- dynButton initBlogId setBlogIdEv deleteButton

    uniqDynBlog       <- holdUniqDyn mDynBlog
    debounceDynBlogEv <- debounce 1 (difference (updated uniqDynBlog) getBlogEv)
    undoEv            <- undoRedo mempty debounceDynBlogEv

    getBlogEv         <- orAlert getResp
    let modBlogEv = leftmost [purePatch <$> getBlogEv, undoEv]

    _               <- orAlert patchEvResult
    deleteEvSuccess <- orAlert deleteEvResult
    postEvSuccess   <- orAlert postEvResult
    replaceLocationUri (fmapMaybe readLocationHeader postEvSuccess)

    dynBlogRemote <- holdDyn
      Nothing
      (leftmost [Just <$> getBlogEv, tagPromptlyDyn dynBlog setBlogIdEv])

    mDynBlog <-
      el "form"
      $    getCompose
      $    pure mempty
      <**> editPk dynBlogId
      <**> editWith textEditor       blogTitleProp       mempty modBlogEv
      <**> editWith checkboxEditor   blogIsExtraProp     mempty modBlogEv
      <**> editWith toggleEditor     blogIsPublishedProp mempty modBlogEv
      <**> editWith (req dateEditor) blogDateProp        mempty modBlogEv
      <**> editWith markdownEditor   blogDescriptionProp mempty modBlogEv

    let dynBlog  = joinPatch <$> mDynBlog

    let dynPatch = makePatch' <$> dynBlogRemote <*> dynBlog
    patchEv <- debounce 3 (() <$ fmapMaybe validAndNonempty (updated dynPatch))

  pure $ coerceUri (linkURI (blogsLink mempty)) <$ leftmost
    [() <$ deleteEvSuccess]
 where
  makePatch' x my = makePatch <$> x <*> my
  validAndNonempty (Just x) | x == mempty = Nothing
                            | otherwise   = Just x
  validAndNonempty Nothing = Nothing

  req = requiredEditor

  dynButton initPk setPk b = switchDyn <$> widgetHold (b initPk) (b <$> setPk)

  getButton _      Nothing   = pure never
  getButton autoEv (Just pk) = requestBtn refreshBtn
                                          (pure . (getBlog pk <$))
                                          (constDyn False)
                                          (constDyn False)
                                          autoEv

  editPk dynPatch = Compose $ pure (setPk <$> dynPatch)
   where
    setPk (Just (BlogId pk)) x = x { _blogId = pure pk }
    setPk Nothing            x = x { _blogId = pure 0 }

  mkHeader (Just (BlogId pk)) = "Blog " <> pack (show pk)
  mkHeader Nothing            = "New blog"

  deleteBlogReq pk ev = do
    ev' <- deleteConfirmation (Set.singleton () <$ ev)
    pure $ deleteBlog usingCookie pk <$ ev'

  deleteButton Nothing   = pure never
  deleteButton (Just pk) = requestBtn deleteBtn
                                      (deleteBlogReq pk)
                                      (constDyn False)
                                      (constDyn False)
                                      never


  patchBlogReq dynPatch pk ev = attachPromptlyDynWithMaybe
    (\x () -> patchBlog usingCookie pk <$> x)
    dynPatch
    ev

  patchButton _        _       Nothing   = pure never
  patchButton dynPatch patchEv (Just pk) = requestBtn
    saveBtn
    (pure . patchBlogReq dynPatch pk)
    ((Just mempty ==) <$> dynPatch)
    ((Nothing ==) <$> dynPatch)
    patchEv


  postBlogReq dynBlog ev = attachPromptlyDynWithMaybe
    (\x () -> fmap (postBlog usingCookie) x)
    dynBlog
    ev

  postButton _       (Just _) = pure never
  postButton dynBlog Nothing  = requestBtn saveBtn
                                           (pure . postBlogReq dynBlog)
                                           (constDyn False)
                                           ((== Nothing) <$> dynBlog)
                                           never

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
