{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Crud.Datagrid
  ( -- * Type conversion
    toApiPage
  , fromApiPage
  , winToApiPage
  , winFromApiPage
  , toApiExceptLimited
  , toApiDirection
  , fromApiDirection
  , fromApiOrdering
  , fromApiView
  , strFilter
  , ordFilter
  , eqFilter
  , setInFilter
  , setNotInFilter
  , setContainsFilter
  , setContains
  -- * Column definition
  , prop
  , gridProp
  , IndexLens(..)
  , filterWith
  , initFilterCondition
  , filterEditor
  -- * Browse form
  , deleteFromMapsubset
  , dynUniqDebounce
  , deleteListButton
  , getListButton
  , getListToMapsubset
  , toMapSubsetDiff
  , BrowseFormConfig(..)
  , browseForm
  , linkWithSelection
  , downloadButtonWithSelection
  ) where

import           Control.Lens                   ( Lens'
                                                , over
                                                , set
                                                , view
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import           Data.Monoid                    ( Last(..) )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , intercalate
                                                )
import           Data.Time                      ( NominalDiffTime )
import           Data.Typeable                  ( Proxy
                                                , Typeable
                                                )
import           GHC.Records                    ( HasField(..) )
import           GHC.TypeLits                   ( KnownSymbol )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import qualified Reflex.Dom.Prerender          as Prerender
                                                ( Client )
import           Servant.API                    ( Link
                                                , ResponseHeader(..)
                                                , getResponse
                                                , lookupResponseHeader
                                                )
import qualified Servant.Crud.API              as API
                                                ( GetListHeaders
                                                , Page(..)
                                                , View'(..)
                                                )
import qualified Servant.Crud.Headers          as API
                                                ( ExceptLimited(..)
                                                , Offset(..)
                                                , TotalCount(..)
                                                )
import qualified Servant.Crud.OrderBy          as API
import           Servant.Crud.QueryOperator
import           URI.ByteString                 ( URI )

import           Common.Api                     ( AttrName
                                                , OrderBy'
                                                , View
                                                )
import           Common.Schema
import           Components.Class
import           Components.Input.Basic
import           Components.Table
import           Frontend.Context               ( AppT )
import           Frontend.Crud.Utils

fromApiView :: View a -> DatagridView API.Path (a Filter)
fromApiView vw = DatagridView { _view_window = winFromApiPage $ API.page vw
                              , _view_sort   = fromApiOrdering $ API.ordering vw
                              , _view_filter = API.filters vw
                              }


toApiPage :: Page -> API.Page
toApiPage p = API.Page
  { API.offset = if _page_num p <= 1
                   then Nothing
                   else Just (_page_size p * (_page_num p - 1))
  , API.limit  = Just (_page_size p)
  }

fromApiPage :: API.Page -> Page
fromApiPage p =
  let pagesize = fromMaybe (pageSize def) (API.limit p)
  in  Page { _page_num  = (fromMaybe 0 (API.offset p) `div` pagesize) + 1
           , _page_size = pagesize
           }

winToApiPage :: ViewWindow -> API.Page
winToApiPage p = API.Page
  { API.offset = if _win_offset p <= 0 then Nothing else Just (_win_offset p)
  , API.limit  = Just (_win_limit p)
  }

winFromApiPage :: API.Page -> ViewWindow
winFromApiPage p =
  let pagesize = fromMaybe 30 (API.limit p)
  in  ViewWindow { _win_offset = fromMaybe 0 (API.offset p)
                 , _win_limit  = pagesize
                 }

toApiExceptLimited :: Selection t -> API.ExceptLimited [t]
toApiExceptLimited (Selection neg xs _)
  | neg       = API.Except (Set.toList xs)
  | otherwise = API.LimitedTo (Set.toList xs)

fromApiDirection :: API.Direction -> SortOrder
fromApiDirection API.Ascending  = Ascending
fromApiDirection API.Descending = Descending

toApiDirection :: SortOrder -> API.Direction
toApiDirection Ascending  = API.Ascending
toApiDirection Descending = API.Descending

fromApiOrdering :: [API.OrderBy c a] -> Map API.Path SortOrder
fromApiOrdering = Map.fromList . fmap
  (\x -> (API.orderByPath x, fromApiDirection (API.orderByDirection x)))

type SetFilter s k a
  = (SetOp (Find s (DefaultFilters a) :: IsInDict s k (DefaultFilters a)))

type SetEqFilter a = (SetFilter "" 'List a, SetFilter "!" 'List a)

type SetOrdFilter a
  = ( SetFilter "gt" 'Normal a
    , SetFilter "lt" 'Normal a
    , SetFilter "ge" 'Normal a
    , SetFilter "le" 'Normal a
    )

type SetStrFilter a
  = ( SetFilter "start" 'Normal a
    , SetFilter "end" 'Normal a
    , SetFilter "contains" 'Normal a
    , SetFilter "!start" 'Normal a
    , SetFilter "!end" 'Normal a
    , SetFilter "!contains" 'Normal a
    )

toApiFilterStr
  :: (SetEqFilter a, SetOrdFilter a, SetStrFilter a)
  => FilterCondition
  -> Last a
  -> Filter a
  -> Filter a
toApiFilterStr f ml = case f of
  StartsWith       -> setf @"start" ml
  EndsWith         -> setf @"end" ml
  Contains         -> setf @"contains" ml
  DoesNotStartWith -> setf @"!start" ml
  DoesNotEndWith   -> setf @"!end" ml
  DoesNotContain   -> setf @"!contains" ml
  _                -> toApiFilterOrd f ml

toApiFilterOrd
  :: (SetEqFilter a, SetOrdFilter a)
  => FilterCondition
  -> Last a
  -> Filter a
  -> Filter a
toApiFilterOrd f ml = case f of
  GreaterThan        -> setf @"gt" ml
  LessThan           -> setf @"lt" ml
  GreaterThanOrEqual -> setf @"ge" ml
  LessThanOrEqual    -> setf @"le" ml
  _                  -> toApiFilterEq f ml

toApiFilterEq
  :: SetEqFilter a => FilterCondition -> Last a -> Filter a -> Filter a
toApiFilterEq f t =
  let ls = maybeToList (getLast t)
  in  case f of
        Equal        -> setf @"" ls
        DoesNotEqual -> setf @"!" ls
        _            -> id

setInFilter :: SetFilter "" 'List a => [a] -> Filter a -> Filter a
setInFilter = setf @""

setNotInFilter :: SetFilter "!" 'List a => [a] -> Filter a -> Filter a
setNotInFilter = setf @"!"

setContainsFilter
  :: SetFilter "contains" 'Normal Text => Last Text -> Filter Text
setContainsFilter x = setf @"contains" x mempty

setContains :: Lens' (a Filter) (C Filter Text) -> Text -> View a -> View a
setContains l c vw =
  vw { API.filters = set l (setContainsFilter (pure c)) (API.filters vw) }

type GetFilter s k a
  = (GetOp (Find s (DefaultFilters a) :: IsInDict s k (DefaultFilters a)))

type GetEqFilter a = (GetFilter "" 'List a, GetFilter "!" 'List a)

type GetOrdFilter a
  = ( GetFilter "gt" 'Normal a
    , GetFilter "lt" 'Normal a
    , GetFilter "ge" 'Normal a
    , GetFilter "le" 'Normal a
    )

type GetStrFilter a
  = ( GetFilter "start" 'Normal a
    , GetFilter "end" 'Normal a
    , GetFilter "contains" 'Normal a
    , GetFilter "!start" 'Normal a
    , GetFilter "!end" 'Normal a
    , GetFilter "!contains" 'Normal a
    )

takeLast :: [a] -> Last a
takeLast = foldMap pure

fromApiFilterStr
  :: (GetEqFilter a, GetOrdFilter a, GetStrFilter a)
  => Filter a
  -> FilterCondition
  -> Last a
fromApiFilterStr f c = case c of
  StartsWith       -> getf @"start" f
  EndsWith         -> getf @"end" f
  Contains         -> getf @"contains" f
  DoesNotStartWith -> getf @"!start" f
  DoesNotEndWith   -> getf @"!end" f
  DoesNotContain   -> getf @"!contains" f
  _                -> fromApiFilterOrd f c

fromApiFilterOrd
  :: (GetEqFilter a, GetOrdFilter a) => Filter a -> FilterCondition -> Last a
fromApiFilterOrd f c = case c of
  GreaterThan        -> getf @"gt" f
  LessThan           -> getf @"lt" f
  GreaterThanOrEqual -> getf @"ge" f
  LessThanOrEqual    -> getf @"le" f
  _                  -> fromApiFilterEq f c

fromApiFilterEq :: GetEqFilter a => Filter a -> FilterCondition -> Last a
fromApiFilterEq f c = case c of
  Equal        -> takeLast $ getf @"" f
  DoesNotEqual -> takeLast $ getf @"!" f
  _            -> mempty

-- | Lens to get and set a (key, Value) pair in f
-- Furthermore a list of available filter conditions, that is,
-- the domain of the getter function
data IndexLens i f b = IndexLens
  { _ilens_get    :: f -> i -> b
  , _ilens_set    :: i -> b -> f -> f
  , _ilens_domain :: [i]
  }

strFilter
  :: ( SetEqFilter a
     , GetEqFilter a
     , SetOrdFilter a
     , GetOrdFilter a
     , SetStrFilter a
     , GetStrFilter a
     )
  => IndexLens FilterCondition (Filter a) (Last a)
strFilter = IndexLens { _ilens_set    = toApiFilterStr
                      , _ilens_get    = fromApiFilterStr
                      , _ilens_domain = [minBound .. maxBound]
                      }

ordFilter
  :: (SetEqFilter a, GetEqFilter a, SetOrdFilter a, GetOrdFilter a)
  => IndexLens FilterCondition (Filter a) (Last a)
ordFilter = IndexLens
  { _ilens_set    = toApiFilterOrd
  , _ilens_get    = fromApiFilterOrd
  , _ilens_domain = [ Equal
                    , DoesNotEqual
                    , GreaterThan
                    , LessThan
                    , GreaterThanOrEqual
                    , LessThanOrEqual
                    ]
  }

eqFilter
  :: (SetEqFilter a, GetEqFilter a)
  => IndexLens FilterCondition (Filter a) (Last a)
eqFilter = IndexLens { _ilens_set    = toApiFilterEq
                     , _ilens_get    = fromApiFilterEq
                     , _ilens_domain = [Equal, DoesNotEqual]
                     }

prop
  :: ( KnownSymbol s
     , HasField s (a AttrName) (C AttrName b)
     , Typeable (a AttrName)
     )
  => Text
  -> (forall f . Lens' (a f) (C f b))
  -> Proxy s
  -> Property a b
prop lbl l p =
  let fn = API.fromHasField p . toApiDirection
  in  Property { _prop_label   = lbl
               , _prop_lens    = l
               , _prop_key     = API.orderByPath (fn Ascending)
               , _prop_orderBy = fn
               }


gridProp
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , Functor c
     , Monoid (OpDict (DefaultFilters b) b)
     )
  => Editor c t m (Maybe b)
  -> IndexLens FilterCondition (Filter b) (Last b)
  -> Property a b
  -> Column t m (OrderBy' a) (a Filter) API.Path (a Identity)
gridProp e' il prp = Column
  { _column_label    = _prop_label prp
  , _column_viewer   = \d ->
    _edit_viewer e (Last . Just . view (_prop_lens prp) <$> d)
  , _column_orderBy  = (_prop_key prp, _prop_orderBy prp)
  , _column_filterBy = ( _ilens_domain il'
                       , initFilterCondition il'
                       , filterEditor e idStr il'
                       )
  }
 where
  il'   = filterWith (_prop_lens prp) il
  e     = coerceEditor Last getLast e'
  idStr = intercalate "." ("filter" : _prop_key prp)

-- | Returns the first filter which is nonzero
initFilterCondition
  :: IndexLens FilterCondition f (Last b) -> f -> FilterCondition
initFilterCondition il f
  | null (_ilens_domain il) = def
  | otherwise = case mapMaybe tryGet (_ilens_domain il) of
    x : _ -> x
    [] -> if def `elem` _ilens_domain il then def else head (_ilens_domain il)
  where tryGet c = fmap (const c) . getLast $ _ilens_get il f c


filterEditor
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Editor c t m (Last b)
  -> Text
  -> IndexLens FilterCondition f (Last b)
  -> f
  -> FilterCondition
  -> Event t FilterCondition
  -> m (Dynamic t (f -> f))
filterEditor e idStr l initF initVal updateC = do
  edtr <- respectFocus
    (_edit_editor e)
    (inputConfig' (_edit_extraConfig e) idStr (_ilens_get l initF initVal))
  dynC <- holdDyn initVal updateC
  pure $ _ilens_set l <$> dynC <*> _inputEl_value edtr

filterWith
  :: Monoid (OpDict (DefaultFilters b) b)
  => Lens' r (Filter b)
  -> IndexLens FilterCondition (Filter b) (Last b)
  -> IndexLens FilterCondition r (Last b)
filterWith l il = il { _ilens_get = _ilens_get il . view l
                     , _ilens_set = \c b -> set l (_ilens_set il c b mempty)
                     }

-- | Delete keys from map subset while moving the others up
deleteFromMapsubset
  :: (Ord (PrimaryKey (BaseTable r) Identity), TableT r)
  => [PrimaryKey (BaseTable r) Identity]
  -> MapSubset Int (r Identity)
  -> MapSubset Int (r Identity)
deleteFromMapsubset xs (MapSubset m c) =
  let xsSet = Set.fromList xs
      (deleted, existing) =
        Map.partition ((`Set.member` xsSet) . getPrimaryKey) m
      deletedIndices  = Map.keysSet deleted
      movedUpExisting = Map.mapKeys
        (\k -> k - Set.size (Set.takeWhileAntitone (< k) deletedIndices))
        existing
  in  MapSubset
        { _ms_data       = movedUpExisting
        , _ms_totalCount = fmap (\x -> x - fromIntegral (Set.size xsSet)) c
        }

-- | @holdDynUniq@ with debounced events
dynUniqDebounce
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , MonadFix m
     , Eq a
     , MonadIO (Performable m)
     )
  => NominalDiffTime
  -> a
  -> Event t a
  -> m (Dynamic t a)
dynUniqDebounce t0 initA setAEv = do
  dynADebounce <- debounce t0 setAEv
  dynA         <- holdDyn initA dynADebounce
  holdUniqDyn dynA

-- | Creates a button which asks for confirmation and then performs the delete request
deleteListButton
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , Prerender t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => (  API.ExceptLimited [c]
     -> a Filter
     -> Request (Prerender.Client (AppT t m)) b
     ) -- ^ The delete request
  -> Dynamic t (Selection c) -- ^ The current selected primary keys
  -> Dynamic t (a Filter) -- ^ The current filter
  -> AppT t m (Event t (Response (Prerender.Client (AppT t m)) b))
deleteListButton req selection dynFilter = requestBtn
  deleteBtn
  deleteReq
  ((<= 0) . _sel_count <$> selection)
  (constDyn False)
  never
 where
  deleteReq ev = do
    ev' <- deleteConfirmation _sel_count (tagPromptlyDyn selection ev)
    pure $ attachPromptlyDynWith (flip req)
                                 dynFilter
                                 (toApiExceptLimited <$> ev')

-- | Creates a load button, which performs the load request on build
getListButton
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , Prerender t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => (a -> Request (Prerender.Client (AppT t m)) b) -- ^ The get request
  -> Dynamic t a -- ^ The current specified view (filters, sort, page)
  -> AppT t m (Event t (Response (Prerender.Client (AppT t m)) b))
getListButton req dynView = do
  pb <- getPostBuild
  requestBtn refreshBtn
             (pure . getReq)
             (constDyn False)
             (constDyn False)
             (leftmost [pb, () <$ updated dynView])
  where getReq = fmap req . tagPromptlyDyn dynView

getListToMapsubset :: API.GetListHeaders a -> MapSubset Int a
getListToMapsubset resp = MapSubset
  (Map.fromList $ zip [x0 ..] (getResponse resp))
  (getCount resp)
 where
  x0 = fromIntegral $ fromMaybe 0 (getOffset resp)
  getCount x =
    case
        lookupResponseHeader x :: ResponseHeader "X-Total-Count" API.TotalCount
      of
        Servant.API.Header (API.TotalCount c) -> Just c
        _ -> Nothing

  getOffset x =
    case lookupResponseHeader x :: ResponseHeader "X-Offset" API.Offset of
      Servant.API.Header (API.Offset c) -> Just c
      _ -> Nothing

-- | Compares the incoming get request results or delete results to the
-- existing rows and constructs a diff which can be used in the datagrid
toMapSubsetDiff
  :: ( MonadFix m
     , MonadHold t m
     , Reflex t
     , Ord (PrimaryKey (BaseTable a) Identity)
     , TableT a
     )
  => Event t (API.GetListHeaders (a Identity)) -- ^ Get result
  -> Event t [PrimaryKey (BaseTable a) Identity] -- ^ Delete result
  -> m (Event t (MapSubset Int (Maybe (a Identity))))
toMapSubsetDiff getEvSuccess deleteEvSuccess = do
  rec
    dynRecRemote <- foldDyn ($) def $ leftmost
      [ const . getListToMapsubset <$> getEvSuccess
      , deleteFromMapsubset <$> deleteEvSuccess
      ]
    keysSet <- holdDyn def (Map.keysSet . _ms_data <$> updateRows)
    let updateRows =
          attachWith addDeletes (current keysSet) (updated dynRecRemote)
  pure updateRows

data BrowseFormConfig t m env a c = BrowseFormConfig
  { _browseConfig_actions
      :: Dynamic t (View a)
      -> Dynamic t (Selection (PrimaryKey (BaseTable a) Identity))
      -> AppT t m c
  , _browseConfig_alerts :: c -> AppT t m (Event t URI)
  , _browseConfig_getListReq
      :: View a
      -> Request
           (Prerender.Client (AppT t m))
           (API.GetListHeaders (a Identity))
  , _browseConfig_deleteListReq
      :: API.ExceptLimited [PrimaryKey (BaseTable a) Identity]
      -> a Filter
      -> Request
           (Prerender.Client (AppT t m))
           [PrimaryKey (BaseTable a) Identity]
  , _browseConfig_header      :: Text
  , _browseConfig_insertRoute :: env -> Link
  , _browseConfig_editRoute :: env -> PrimaryKey (BaseTable a) Identity -> Link
  , _browseConfig_browseRoute :: env -> View a -> Link
  , _browseConfig_columns
      :: [Column t (AppT t m) (OrderBy' a) (a Filter) API.Path (a Identity)]
  }

browseForm
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Prerender t m
     , MonadFix m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     , Ord (PrimaryKey (BaseTable a) Identity)
     , Eq (a Filter)
     , TableT a
     )
  => Dynamic t (Maybe env)
  -> BrowseFormConfig t m env a c
  -> View a
  -> AppT t m (Event t URI)
browseForm env cfg vw = elClass "div" "flex-column" $ do
  el "h1" $ text (_browseConfig_header cfg)

  rec
    (insertEvResult, getEvResult, deleteEvResult, customEv) <-
      el "div"
      $   (,,,)
      <$> optionalBtn insertBtn (fmap (_browseConfig_insertRoute cfg) <$> env)
      <*> getListButton (_browseConfig_getListReq cfg) dynView
      <*> deleteListButton (_browseConfig_deleteListReq cfg) selection dynFilter
      <*> _browseConfig_actions cfg dynView selection

    deleteEvSuccess <- orAlert deleteEvResult
    getEvSuccess    <- orAlert getEvResult
    customEvSuccess <- _browseConfig_alerts cfg customEv

    updateRows      <- toMapSubsetDiff getEvSuccess deleteEvSuccess

    gridResult      <- datagridDyn DatagridConfig
      { _gridConfig_columns     = _browseConfig_columns cfg
      , _gridConfig_selectAll   = leftmost
        [False <$ deleteEvSuccess, False <$ updated dynFilter]
      , _gridConfig_setValue    = updateRows
      , _gridConfig_toLink      = \r ->
        fmap (\v -> _browseConfig_editRoute cfg v (getPk r)) <$> env
      , _gridConfig_initialView = fromApiView vw
      , _gridConfig_toPrimary   = getPk
      }

    let selection = _grid_selection gridResult
    dynPage   <- holdUniqDyn $ winToApiPage <$> _grid_window gridResult
    dynSort   <- holdUniqDyn $ _grid_columns gridResult
    dynFilter <- dynUniqDebounce 1 (API.filters vw)
      $ updated (_grid_filter gridResult)
    let dynView = API.View <$> dynPage <*> dynSort <*> dynFilter
    replaceLocation
      (attachPromptlyDynWithMaybe browseRoute env (updated dynView))

  pure (leftmost [_grid_navigate gridResult, insertEvResult, customEvSuccess])

 where
  browseRoute (Just x) y = Just $ _browseConfig_browseRoute cfg x y
  browseRoute Nothing  _ = Nothing

  getPk = getPrimaryKey

linkWithSelection
  :: (SetFilter "" 'List b, SetFilter "!" 'List b)
  => Lens' (a Filter) (Filter b)
  -> (PrimaryKey (BaseTable a) Identity -> b)
  -> (View a -> Link)
  -> View a
  -> Selection (PrimaryKey (BaseTable a) Identity)
  -> Link
linkWithSelection l unId toLnk v (Selection neg pks _) =
  let setFilterFn = if neg then setNotInFilter else setInFilter
      pks'        = unId <$> Set.toList pks
  in  toLnk $ v { API.filters = over l (setFilterFn pks') (API.filters v) }

downloadButtonWithSelection
  :: ( DomBuilder t m
     , PostBuild t m
     , SetFilter "" 'List b
     , SetFilter "!" 'List b
     )
  => Lens' (a Filter) (Filter b)
  -> (PrimaryKey (BaseTable a) Identity -> b)
  -> (View a -> Link)
  -> Dynamic t (View a)
  -> Dynamic t (Selection (PrimaryKey (BaseTable a) Identity))
  -> m ()
downloadButtonWithSelection l unId toLnk dynVw dynSel =
  downloadButton (linkWithSelection l unId toLnk <$> dynVw <*> dynSel)
