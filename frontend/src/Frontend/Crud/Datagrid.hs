{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
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
  , strFilter
  , ordFilter
  , eqFilter
  , setInFilter
  , setNotInFilter
  -- * Column definition
  , prop
  , gridProp
  , IndexLens(..)
  , filterWith
  ) where

import           Control.Lens                   ( Lens'
                                                , set
                                                , view
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           Data.Typeable                  ( Proxy
                                                , Typeable
                                                )
import           GHC.Records                    ( HasField(..) )
import           GHC.TypeLits                   ( KnownSymbol )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import qualified Servant.Crud.API              as API
                                                ( Page(..) )
import qualified Servant.Crud.Headers          as API
                                                ( ExceptLimited(..) )
import qualified Servant.Crud.OrderBy          as API
import           Servant.Crud.QueryOperator

import           Common.Api                     ( Be
                                                , OrderByScope
                                                , ViewOrderBy
                                                )
import           Common.Schema                  ( C )
import           Components.Input.Basic
import           Components.Table
import           Frontend.Crud.Utils



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
  -> MaybeLast a
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
  -> MaybeLast a
  -> Filter a
  -> Filter a
toApiFilterOrd f ml = case f of
  GreaterThan        -> setf @"gt" ml
  LessThan           -> setf @"lt" ml
  GreaterThanOrEqual -> setf @"ge" ml
  LessThanOrEqual    -> setf @"le" ml
  _                  -> toApiFilterEq f ml

toApiFilterEq
  :: SetEqFilter a => FilterCondition -> MaybeLast a -> Filter a -> Filter a
toApiFilterEq f t =
  let ls = maybeToList (unMaybeLast t)
  in  case f of
        Equal        -> setf @"" ls
        DoesNotEqual -> setf @"!" ls
        _            -> id

setInFilter :: SetFilter "" 'List a => [a] -> Filter a -> Filter a
setInFilter = setf @""

setNotInFilter :: SetFilter "!" 'List a => [a] -> Filter a -> Filter a
setNotInFilter = setf @"!"

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
  => IndexLens FilterCondition (Filter a) (MaybeLast a)
strFilter = IndexLens { _ilens_set    = toApiFilterStr
                      , _ilens_get    = fromApiFilterStr
                      , _ilens_domain = [minBound .. maxBound]
                      }

ordFilter
  :: (SetEqFilter a, GetEqFilter a, SetOrdFilter a, GetOrdFilter a)
  => IndexLens FilterCondition (Filter a) (MaybeLast a)
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
  => IndexLens FilterCondition (Filter a) (MaybeLast a)
eqFilter = IndexLens { _ilens_set    = toApiFilterEq
                     , _ilens_get    = fromApiFilterEq
                     , _ilens_domain = [Equal, DoesNotEqual]
                     }

prop
  :: ( KnownSymbol s
     , HasField s (a (OrderByScope Be)) (C (OrderByScope Be) b)
     , Typeable (a (OrderByScope Be))
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
  -> IndexLens FilterCondition (Filter b) (MaybeLast b)
  -> Property a b
  -> Column t m (ViewOrderBy Be a) (a Filter) API.Path (a Identity)
gridProp e' il prp = Column
  { _column_label    = _prop_label prp
  , _column_viewer   = \d ->
    _edit_viewer e (MaybeLast . Just . view (_prop_lens prp) <$> d)
  , _column_orderBy  = (_prop_key prp, _prop_orderBy prp)
  , _column_filterBy = ( _ilens_domain il'
                       , initFilterCondition il'
                       , filterEditor e il'
                       )
  }
 where
  il' = filterWith (_prop_lens prp) il
  e   = coerceEditor MaybeLast unMaybeLast e'

-- | Returns the first filter which is nonzero
initFilterCondition
  :: IndexLens FilterCondition f (MaybeLast b) -> f -> FilterCondition
initFilterCondition il f
  | null (_ilens_domain il) = def
  | otherwise = case mapMaybe tryGet (_ilens_domain il) of
    x : _ -> x
    [] -> if def `elem` _ilens_domain il then def else head (_ilens_domain il)
  where tryGet c = fmap (const c) . unMaybeLast $ _ilens_get il f c


filterEditor
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Editor c t m (MaybeLast b)
  -> IndexLens FilterCondition f (MaybeLast b)
  -> f
  -> FilterCondition
  -> Event t FilterCondition
  -> m (Dynamic t (f -> f))
filterEditor e l initF initVal updateC = do
  edtr <- respectFocus
    (_edit_editor e)
    (inputConfig' (_edit_extraConfig e) (_ilens_get l initF initVal))
  dynC <- holdDyn initVal updateC
  pure $ _ilens_set l <$> dynC <*> _inputEl_value edtr

filterWith
  :: Monoid (OpDict (DefaultFilters b) b)
  => Lens' r (Filter b)
  -> IndexLens FilterCondition (Filter b) (MaybeLast b)
  -> IndexLens FilterCondition r (MaybeLast b)
filterWith l il = il { _ilens_get = _ilens_get il . view l
                     , _ilens_set = \c b -> set l (_ilens_set il c b mempty)
                     }
