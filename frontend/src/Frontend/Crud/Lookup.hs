{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Crud.Lookup
  ( lookupInput
  , LabelEndpoint(..)
  , labelEndpoint
  , LookupInputConfig
  , FkProperty(..)
  , fkProp
  , editFk
  , gridFkProp
  ) where

import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( Lens'
                                                , set
                                                , view
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Functor.Const             ( Const(..) )
import           Data.Functor.Identity          ( Identity(..) )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Last(..) )
import           Data.Text                      ( Text
                                                , intercalate
                                                )
import           Data.Typeable                  ( Proxy(..)
                                                , Typeable
                                                )
import           GHC.Records                    ( HasField(..) )
import           GHC.TypeLits                   ( KnownSymbol )
import           Reflex.Dom
import qualified Reflex.Dom.Prerender          as Prerender
                                                ( Client )
import           Servant.API                    ( toUrlPiece )
import qualified Servant.Crud.API              as API
                                                ( GetListHeaders
                                                , Page(..)
                                                , page
                                                )
import qualified Servant.Crud.OrderBy          as API
import           Servant.Crud.QueryOperator     ( Filter )
import qualified Servant.Links                 as L
                                                ( Link
                                                , linkURI
                                                )
import           Servant.Subscriber.Reflex      ( ClientError
                                                , requestingJs
                                                )
import           URI.ByteString                 ( URI )

import           Common.Api                     ( AttrName
                                                , OrderBy'
                                                , View
                                                )
import           Common.Schema
import           Components.Input
import           Components.Navigation
import           Components.Table
import           Frontend.Crud.Datagrid
import           Frontend.Crud.Utils

type LookupInputConfig t m env k = InputConfig' (LabelEndpoint t m env k) t m

data LabelEndpoint t m env k a = LabelEndpoint
  { unLabelEndpoint
      :: OptionsRequest k
      -> Request (Prerender.Client m) (API.GetListHeaders (Elem a))
  , editLink :: env -> k -> L.Link
  }

lookupInput
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , Prerender t m
     , Ord (PrimaryKey a Identity)
     , EventWriter t (Last URI) m
     , Requester t (Prerender.Client m)
     , Response (Client m)  ~ Either ClientError
     )
  => Dynamic t (Maybe env)
  -> LookupInputConfig
       t
       m
       env
       (PrimaryKey a Identity)
       (Maybe (Named a Identity))
  -> m (DomInputEl t m (Maybe (Named a Identity)))
lookupInput env cfg = do
  rec (x, routeEv) <- comboboxInputKS
        (openlinkEl (_cb_selection <$> _inputEl_value x))
        showOpt
        def
        requestOptions
        (fmap toCbValue cfg { _inputConfig_extra = Const () })

  tellEvent (Last . Just <$> routeEv)

  pure (fmap fromCbValue x)

 where
  requestOptions ev = do
    dynReq       <- holdDyn Nothing $ Just <$> ev
    dynReqUniq   <- holdUniqDyn dynReq
    dynReqTrottl <- debounce 1 (updated dynReqUniq)
    let reqEv = endpoint <$> fmapMaybe Prelude.id dynReqTrottl
    resultEv   <- requestingJs reqEv
    loadingDyn <- holdDyn False $ leftmost [False <$ resultEv, True <$ reqEv]
    pure
      ( either (Left . fst . showClientError)
               (Right . fmap toTuple . getListToMapsubset)
        <$> resultEv
      , loadingDyn
      )

  toTuple n = (_id n, _name n)

  endpoint = unLabelEndpoint (_inputConfig_extra cfg)
  mklnk    = editLink (_inputConfig_extra cfg)

  showOpt _ = dynText

  toCbValue
    :: Maybe (Named a Identity) -> ComboboxValue (Maybe (PrimaryKey a Identity))
  toCbValue (Just n) =
    ComboboxValue { _cb_selection = Just (_id n), _cb_text = _name n }
  toCbValue Nothing = ComboboxValue { _cb_selection = Nothing, _cb_text = "" }

  fromCbValue
    :: ComboboxValue (Maybe (PrimaryKey a Identity)) -> Maybe (Named a Identity)
  fromCbValue ComboboxValue { _cb_selection = mx, _cb_text = t } =
    (\pk -> Named { _id = pk, _name = t }) <$> mx

  openlinkEl mkey = do
    mkeyDyn <- maybeDyn (liftA2 (,) <$> env <*> mkey)
    dynEv   <- dyn
      (   maybe (pure never)
                (safelinkNoTab angleDoubleRightIcon . fmap (uncurry mklnk))
      <$> mkeyDyn
      )
    switchHold never dynEv

safelinkNoTab
  :: (PostBuild t m, DomBuilder t m)
  => m ()
  -> Dynamic t L.Link
  -> m (Event t URI)
safelinkNoTab cnt lnk = do
  clickEv <- ahrefPreventDefault
    (("/" <>) . toUrlPiece <$> lnk)
    (constDyn False)
    (  Map.singleton "tabindex" "-1"
    <> Map.singleton "style" "padding-top:0.125rem"
    )
    cnt
  pure $ tagPromptlyDyn (coerceUri . L.linkURI <$> lnk) clickEv

labelEndpoint
  :: (Monoid (a Filter))
  => Lens' (a Filter) (C Filter Text)
  -> (  View a
     -> Maybe (PrimaryKey a Identity)
     -> Request
          (Prerender.Client m)
          (API.GetListHeaders (Named a Identity))
     )
  -> (env -> PrimaryKey a Identity -> L.Link)
  -> LabelEndpoint
       t
       m
       env
       (PrimaryKey a Identity)
       (Maybe (Named a Identity))
labelEndpoint l req = LabelEndpoint mkRequest
 where
  mkRequest (SearchText t (off, lim)) =
    req (setContains l t $ pageVw (<=) off lim) Nothing
  mkRequest (NearSelection (Just x) lim) =
    req (pageVw (==) (-lim `div` 2) lim) (Just x)
  mkRequest (NearSelection Nothing lim) =
    req (pageVw (==) (0 :: Integer) lim) Nothing

  pageVw cond off lim = mempty
    { API.page = API.Page
                   { API.offset = if off `cond` 0
                                    then Nothing
                                    else Just (fromIntegral off)
                   , API.limit  = Just . fromIntegral $ lim
                   }
    }


data FkProperty t m env a b = FkProperty
  { _fkProp_label   :: Text
  , _fkProp_lens    :: forall f . Lens' (a f) (Named b f)
  , _fkProp_key     :: API.Path
  , _fkProp_orderBy :: SortOrder -> OrderBy' a
  , _fkProp_endpoint
      :: View b
      -> Maybe (PrimaryKey b Identity)
      -> Request (Prerender.Client m) (API.GetListHeaders (Named b Identity))
  , _fkProp_searchField :: Lens' (b Filter) (C Filter Text)
  , _fkProp_editLink    :: env -> PrimaryKey b Identity -> L.Link
  }

fkProp
  :: ( KnownSymbol s
     , HasField s (a AttrName) (Named b AttrName)
     , Typeable (a AttrName)
     , Typeable b
     )
  => Text
  -> (forall f . Lens' (a f) (Named b f))
  -> Proxy s
  -> Lens' (b Filter) (C Filter Text)
  -> (  View b
     -> Maybe (PrimaryKey b Identity)
     -> Request
          (Prerender.Client m)
          (API.GetListHeaders (Named b Identity))
     )
  -> (env -> PrimaryKey b Identity -> L.Link)
  -> FkProperty t m env a b
fkProp lbl l p sf ep lnk =
  let fn = API.prependHasField p . sortByName . toApiDirection
  in  FkProperty { _fkProp_label       = lbl
                 , _fkProp_lens        = l
                 , _fkProp_key         = API.orderByPath (fn Ascending)
                 , _fkProp_orderBy     = fn
                 , _fkProp_searchField = sf
                 , _fkProp_editLink    = lnk
                 , _fkProp_endpoint    = ep
                 }
  where sortByName = API.fromHasField (Proxy :: Proxy "_name")

editFk
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , MonadIO (Performable m)
     , Monoid (b Filter)
     , Ord (PrimaryKey b Identity)
     , PerformEvent t m
     , PostBuild t m
     , Prerender t m
     , TriggerEvent t m
     , Beamable (PrimaryKey b)
     , Monoid (PrimaryKey b Last)
     , EventWriter t (Last URI) m
     , Requester t (Prerender.Client m)
     , Response (Client m)  ~ Either ClientError
     )
  => Dynamic t (Maybe env)
  -> FkProperty t m env a b
  -> Event t (a Last)
  -> Compose m (Dynamic t) (a Last -> a Last)
editFk env prp setEv = Compose $ fmap mksetter . _inputEl_value <$> labeled
  (_fkProp_label prp)
  (respectFocus (lookupInput env))
  (inputConfig'
      (labelEndpoint (_fkProp_searchField prp)
                     (_fkProp_endpoint prp)
                     (_fkProp_editLink prp)
      )
      (intercalate "." (_fkProp_key prp))
      Nothing
    )
    { _inputConfig_setValue = viewer <$> setEv
    }
 where
  mksetter (Just x) = set (_fkProp_lens prp) (purePatch x)
  mksetter Nothing  = set (_fkProp_lens prp) mempty

  viewer x = case view (_fkProp_lens prp) x of
    Named mpk mlbl ->
      fmap (`Named` fromMaybe "?" (getLast mlbl)) (joinPatch mpk)

gridFkProp
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => FkProperty t m env a b
  -> Column t m (OrderBy' a) (a Filter) API.Path (a Identity)
gridFkProp prp = Column
  { _column_label    = _fkProp_label prp
  , _column_viewer   = \d -> _edit_viewer e (Last . Just . view l <$> d)
  , _column_orderBy  = (_fkProp_key prp, _fkProp_orderBy prp)
  , _column_filterBy = ( _ilens_domain il'
                       , initFilterCondition il'
                       , filterEditor e idStr il'
                       )
  }
 where
  il'   = filterWith (_fkProp_lens prp . name) strFilter
  e     = coerceEditor Last getLast textEditor

  l     = _fkProp_lens prp . name
  idStr = intercalate "." ("filter" : _fkProp_key prp)


name :: Lens' (Named a f) (C f Text)
name inj (Named k l) = Named k <$> inj l
{-# INLINE name #-}
