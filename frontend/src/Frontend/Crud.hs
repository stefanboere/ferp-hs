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
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           GHC.Int                        ( Int64 )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Crud.API               ( View'(..) )
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
import           Frontend.Crud.Edit
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

  rec
    let getBlogReq = fmap getBlogs . tagPromptlyDyn dynView
    (insertEv, getBlogEv, mDeleteEvResult) <- el "div" $ do
      insertEv'  <- insertBtn (constDyn newBlogLink)
      getBlogEv' <- requestBtn refreshBtn
                               (pure . getBlogReq)
                               (constDyn False)
                               (constDyn False)
                               (leftmost [postBuildEv, () <$ updated dynView])

      let deleteBlogReq ev = do
            ev' <- deleteConfirmation _sel_count (tagPromptlyDyn selection ev)
            pure $ attachPromptlyDynWith (flip $ deleteBlogs usingCookie)
                                         dynFilterUniq
                                         (toApiExceptLimited <$> ev')

      mDeleteEvResult' <- requestBtn deleteBtn
                                     deleteBlogReq
                                     ((<= 0) . _sel_count <$> selection)
                                     (constDyn False)
                                     never

      downloadButton (linkWithSelection <$> dynView <*> selection)
      pure (insertEv', getBlogEv', mDeleteEvResult')

    deleteEvResult <- orAlert mDeleteEvResult

    recEv          <- orAlert getBlogEv
    blogsDynRemote <- foldDyn ($) def $ leftmost
      [const . getListToMapsubset <$> recEv, doDeletes <$> deleteEvResult]
    keysSet <- holdDyn def (Map.keysSet . _ms_data <$> updateRows)
    let updateRows =
          attachWith addDeletes (current keysSet) (updated blogsDynRemote)

    gridResult <- datagridDyn DatagridConfig
      { _gridConfig_columns       =
        [ gridProp noEditor   eqFilter  blogIdProp
        , gridProp textEditor strFilter blogTitleProp
        , gridProp textEditor strFilter blogDescriptionProp
        ]
      , _gridConfig_selectAll     = leftmost
        [False <$ deleteEvResult, False <$ updated dynFilterUniq]
      , _gridConfig_setValue      = updateRows
      , _gridConfig_toLink        = blogLink . primaryKey
      , _gridConfig_initialWindow = winFromApiPage $ page vw
      , _gridConfig_toPrimary     = primaryKey
      , _gridConfig_initialSort   = fromApiOrdering $ ordering vw
      , _gridConfig_initialFilter = filters vw
      }

    let selection = _grid_selection gridResult
    dynPage           <- holdUniqDyn $ winToApiPage <$> _grid_window gridResult
    dynSort           <- holdUniqDyn $ _grid_columns gridResult
    dynFilterDebounce <- debounce 1 $ updated (_grid_filter gridResult)
    dynFilter         <- holdDyn (filters vw) dynFilterDebounce
    dynFilterUniq     <- holdUniqDyn dynFilter
    let dynView = View <$> dynPage <*> dynSort <*> dynFilterUniq
    replaceLocation (blogsLink <$> updated dynView)

  pure (leftmost [_grid_navigate gridResult, insertEv])

 where
  linkWithSelection v (Selection neg pks _) =
    let setFilterFn = if neg then setNotInFilter else setInFilter
        pks'        = unBlogId <$> Set.toList pks
    in  getBlogsApiLink
          $ v { filters = over blogId (setFilterFn pks') (filters v) }

  unBlogId (BlogId x) = x

  doDeletes xs (MapSubset m c) =
    let xsSet = Set.fromList xs
        (deleted, existing) =
          Map.partition ((`Set.member` xsSet) . primaryKey) m
        deletedIndices  = Map.keysSet deleted
        movedUpExisting = Map.mapKeys
          (\k -> k - Set.size (Set.takeWhileAntitone (< k) deletedIndices))
          existing
    in  MapSubset
          { _ms_data       = movedUpExisting
          , _ms_totalCount = fmap (\x -> x - fromIntegral (Set.size xsSet)) c
          }

blogEdit
  :: WidgetConstraint js t m => Maybe BlogId -> ApiWidget t m (Event t URI)
blogEdit = editForm cfg $ \modBlogEv ->
  el "form"
    $    getCompose
    $    pure mempty
    <**> editWith textEditor       blogTitleProp       modBlogEv
    <**> editWith checkboxEditor   blogIsExtraProp     modBlogEv
    <**> editWith toggleEditor     blogIsPublishedProp modBlogEv
    <**> editWith (req dateEditor) blogDateProp        modBlogEv
    <**> editWith markdownEditor   blogDescriptionProp modBlogEv
 where
  cfg = EditFormConfig { _formConfig_actions          = \_ _ _ _ -> pure never
                       , _formConfig_getReq           = getBlog
                       , _formConfig_deleteReq        = deleteBlog usingCookie
                       , _formConfig_postReq          = postBlog usingCookie
                       , _formConfig_patchReq         = patchBlog usingCookie
                       , _formConfig_setPrimaryKey    = setBlogPk
                       , _formConfig_header           = mkHeader
                       , _formConfig_routeAfterDelete = blogsLink mempty
                       , _formConfig_editRoute        = blogLink
                       }
  req = requiredEditor

  setBlogPk (Just (BlogId pk)) x = x { _blogId = pure pk }
  setBlogPk Nothing            x = x { _blogId = pure 0 }

  mkHeader x
    | fromMaybe 0 (unMaybeLast (_blogId x)) > 0 = "Blog - "
    <> fromMaybe "?" (unMaybeLast $ _blogName x)
    | otherwise = "New blog"

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
