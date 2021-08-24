{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Crud.Datagrid
  ( toApiPage
  , fromApiPage
  , replaceLocation
  , toApiDirection
  , fromApiDirection
  , fromApiOrdering
  , strFilter
  , ordFilter
  , eqFilter
  ) where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           GHCJS.DOM.Types                ( SerializedScriptValue(..) )
import           Language.Javascript.JSaddle    ( pToJSVal )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import qualified Servant.Crud.API              as API
                                                ( Page(..) )
import qualified Servant.Crud.OrderBy          as API
import           Servant.Crud.QueryOperator
import           Servant.Links                  ( Link
                                                , URI(..)
                                                , linkURI
                                                )

import           Components.Table



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

replaceLocation
  :: (TriggerEvent t m, PerformEvent t m, Prerender js t m)
  => Event t Link
  -> m ()
replaceLocation lEv = prerender_ (pure ()) $ do
  _ <- manageHistory
    (HistoryCommand_ReplaceState . historyItem . linkURI <$> lEv)
  pure ()
 where
  historyItem :: URI -> HistoryStateUpdate
  historyItem uri = HistoryStateUpdate
    { _historyStateUpdate_state = SerializedScriptValue (pToJSVal False)
    , _historyStateUpdate_title = ""
    , _historyStateUpdate_uri   = Just uri { uriPath = "/" <> uriPath uri }
    }

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
  -> a
  -> Filter a
  -> Filter a
toApiFilterStr f t =
  let ml = MaybeLast (Just t)
  in  case f of
        StartsWith       -> setf @"start" ml
        EndsWith         -> setf @"end" ml
        Contains         -> setf @"contains" ml
        DoesNotStartWith -> setf @"!start" ml
        DoesNotEndWith   -> setf @"!end" ml
        DoesNotContain   -> setf @"!contains" ml
        _                -> toApiFilterOrd f t

toApiFilterOrd
  :: (SetEqFilter a, SetOrdFilter a)
  => FilterCondition
  -> a
  -> Filter a
  -> Filter a
toApiFilterOrd f t =
  let ml = MaybeLast (Just t)
  in  case f of
        GreaterThan        -> setf @"gt" ml
        LessThan           -> setf @"lt" ml
        GreaterThanOrEqual -> setf @"ge" ml
        LessThanOrEqual    -> setf @"le" ml
        _                  -> toApiFilterEq f t

toApiFilterEq :: SetEqFilter a => FilterCondition -> a -> Filter a -> Filter a
toApiFilterEq f t = case f of
  Equal        -> setf @"" [t]
  DoesNotEqual -> setf @"!" [t]
  _            -> id

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

takeLast :: [a] -> MaybeLast a
takeLast = foldMap pure

fromApiFilterStr
  :: (GetEqFilter a, GetOrdFilter a, GetStrFilter a)
  => Filter a
  -> FilterCondition
  -> MaybeLast a
fromApiFilterStr f c = case c of
  StartsWith       -> getf @"start" f
  EndsWith         -> getf @"end" f
  Contains         -> getf @"contains" f
  DoesNotStartWith -> getf @"!start" f
  DoesNotEndWith   -> getf @"!end" f
  DoesNotContain   -> getf @"!contains" f
  _                -> fromApiFilterOrd f c

fromApiFilterOrd
  :: (GetEqFilter a, GetOrdFilter a)
  => Filter a
  -> FilterCondition
  -> MaybeLast a
fromApiFilterOrd f c = case c of
  GreaterThan        -> getf @"gt" f
  LessThan           -> getf @"lt" f
  GreaterThanOrEqual -> getf @"ge" f
  LessThanOrEqual    -> getf @"le" f
  _                  -> fromApiFilterEq f c

fromApiFilterEq :: GetEqFilter a => Filter a -> FilterCondition -> MaybeLast a
fromApiFilterEq f c = case c of
  Equal        -> takeLast $ getf @"" f
  DoesNotEqual -> takeLast $ getf @"!" f
  _            -> mempty

strFilter
  :: ( SetEqFilter a
     , GetEqFilter a
     , SetOrdFilter a
     , GetOrdFilter a
     , SetStrFilter a
     , GetStrFilter a
     )
  => IndexLens FilterCondition (Filter a) a
strFilter = IndexLens { _ilens_set    = toApiFilterStr
                      , _ilens_get    = \x -> unMaybeLast . fromApiFilterStr x
                      , _ilens_domain = [minBound .. maxBound]
                      }

ordFilter
  :: (SetEqFilter a, GetEqFilter a, SetOrdFilter a, GetOrdFilter a)
  => IndexLens FilterCondition (Filter a) a
ordFilter = IndexLens
  { _ilens_set    = toApiFilterOrd
  , _ilens_get    = \x -> unMaybeLast . fromApiFilterOrd x
  , _ilens_domain = [ Equal
                    , DoesNotEqual
                    , GreaterThan
                    , LessThan
                    , GreaterThanOrEqual
                    , LessThanOrEqual
                    ]
  }

eqFilter
  :: (SetEqFilter a, GetEqFilter a) => IndexLens FilterCondition (Filter a) a
eqFilter = IndexLens { _ilens_set    = toApiFilterEq
                     , _ilens_get    = \x -> unMaybeLast . fromApiFilterEq x
                     , _ilens_domain = [Equal, DoesNotEqual]
                     }
