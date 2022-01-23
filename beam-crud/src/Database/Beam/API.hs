{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module: Database.Beam.API
Description: Beam implementation of Servant.Crud.API

This module provides the 'setView' function, to alter a query to match the requested view.

Furthermore, it provides utilities for default implementations of certain routes.

-}
module Database.Beam.API
  ( -- * Views
    -- | These functions are of the form @parameter -> (Q be s a -> Q be s a)@,
    -- i.e. given a parameter, they transform existing queries.
    --
    -- For better understanding you can ignore 'QNested' and 'WithRewrittenThread' in the types.
    setView
  , View
  , orderByO_
  , setPage
    -- * Types
  , GetList
  , GetListLabels
  , Get_
  , Put_
  , Patch_
  , Delete_
  , DeleteList_
  , Post_
  , PostList
  , CaptureId
    -- * Utilities
  , runSetView
  , runSetViewAround
  , runCountInView
  , runPrimaryKeysInView
  , runNamesInViewWithCount
  , runNamesInViewWithCountAround
    -- * Advanced
  , Orderable
  , OrderBy'
  , ViewOrderBy
  , OrderByScope
  )
where

import           Prelude

import           Control.Monad.Free             ( liftF )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Last )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import           Database.Beam
import           Database.Beam.Backend.SQL      ( BeamSql2003ExpressionBackend
                                                , BeamSql99CommonTableExpressionBackend
                                                , BeamSqlBackend
                                                , BeamSqlBackendCanSerialize
                                                , BeamSqlBackendOrderingSyntax
                                                )
import           Database.Beam.Expand           ( Named(..)
                                                , ToName(..)
                                                )
import           Database.Beam.Extra            ( runCountIn
                                                , runSelectReturningListWithCount
                                                )
import           Database.Beam.Operator         ( FieldsFulfillConstraintFilter
                                                , Operator
                                                , matching_
                                                )
import           Database.Beam.Query.CTE        ( QAnyScope )
import           Database.Beam.Query.Internal
import           GHC.Generics                   ( Rep )
import           Servant.API
import           Servant.Crud.API
import           Servant.Crud.Headers           ( ExceptLimited(..)
                                                , PathInfo
                                                , TotalCount(..)
                                                )
import           Servant.Crud.OrderBy           ( Direction(..)
                                                , GSelectors
                                                , HSelector(..)
                                                , OrderBy(..)
                                                , Selectors(..)
                                                , defaultSelectors
                                                , leaf
                                                )
import           Servant.Crud.QueryObject       ( Options(..)
                                                , QObj
                                                , defaultOptions
                                                )
import           Servant.Crud.QueryOperator     ( Filter )


-- | 'OrderBy' specialized for use with the Beam backend
type OrderBy' be s t = OrderBy (Orderable be) (t (QExpr be s))

-- | @OrderBy'@ with specialized scope
type ViewOrderBy be t = OrderBy (Orderable be) (t (OrderByScope be))

type OrderByScope be
  = QExpr be (QNested (QNested (QNested (QNested QBaseScope))))

-- | Contains ordering, filtering and pageination info. This particular type works
-- nice with 'setView'. You can use this type with 'QueryObject' to allow the client
-- to specify ordering, filtering and pageination info in the request using request
-- parameters.
--
-- Then, in the handler, simply use 'setView' to alter the query to match this requested view.
type View be t = View' (Orderable be) (t (OrderByScope be)) (t Filter)

-- | Endpoint returning a list of @t Identity@ based on a @View@ in the request query parameters
--
-- E.g. the type of the @\/books@ endpoint.
type GetList be t = PathInfo :> QObj (View be t) :> GetList' (t Identity)

-- | Subroute compared to @GetList@ which only returns the primary keys and labels
type GetListLabels be t
  = "labels" :> PathInfo :> QObj (View be t) :> QueryParam "around" (PrimaryKey t Identity) :> Get '[JSON] (GetListHeaders (Named t Identity))

-- | Regular get requests
type Get_ t = CaptureId t :> Get' (t Identity)

-- | Regular patch requests
type Patch_ t = CaptureId t :> Req' (t Last) :> Patch_'

-- | Regular put requests
type Put_ t = CaptureId t :> Req' (t Identity) :> Put_'

-- | Regular delete requests
type Delete_ t = CaptureId t :> Delete_'

-- | Delete list requests
type DeleteList_ be t
  = Req' (ExceptLimited [PrimaryKey t Identity]) :> QObj (t Filter) :> Delete '[JSON] [PrimaryKey t Identity]

-- | Regular post requests
type Post_ t
  = PathInfo :> "0" :> Req' (t Identity) :> Post_' (PrimaryKey t Identity)

-- | Posting many records
type PostList t
  = ReqCSV' [t Identity] :> PostCreated '[JSON] [PrimaryKey t Identity]

-- | Id in the request path
type CaptureId t = Capture "id" (PrimaryKey t Identity)

-- | Expressions which can be turned into order syntax.
--
-- This is exposed to allow the end user to specify more general 'View' types.
class Orderable be expr where
  toOrdering :: Proxy be -> Direction -> expr -> WithExprContext (BeamSqlBackendOrderingSyntax be)

instance BeamSqlBackend be => Orderable be (QGenExpr QValueContext be s a) where
  toOrdering _ Ascending  = toExpr . asc_
  toOrdering _ Descending = toExpr . desc_

toExpr :: QOrd be s a -> WithExprContext (BeamSqlBackendOrderingSyntax be)
toExpr (QOrd x) = x

instance BeamSqlBackend be => Selectors (Orderable be) (QGenExpr QValueContext be s a) where
  selectors = leaf

type PrimaryKeySelectorsConstraint be t s
  = ( Generic (PrimaryKey t (QGenExpr QValueContext be s))
    , GSelectors
        (Orderable be)
        (Rep (PrimaryKey t (QGenExpr QValueContext be s)))
    , BeamSqlBackend be
    )

instance PrimaryKeySelectorsConstraint be t s
    => Selectors (Orderable be) (Named t (QGenExpr QValueContext be s))

instance
    PrimaryKeySelectorsConstraint be t s
  => Selectors (Orderable be) (PrimaryKey t (QGenExpr QValueContext be s)) where
  selectors = defaultSelectors
    (defaultOptions { fieldLabelModifier     = const ""
                    , constructorTagModifier = const ""
                    }
    )

extract'
  :: forall r be s s1
   . (Beamable r)
  => Proxy be
  -> r (QExpr be s)
  -> OrderBy (Orderable be) (r (QExpr be s1))
  -> WithExprContext (BeamSqlBackendOrderingSyntax be)
extract' p r ord = toVal (orderBySelector ord)
 where
  toVal
    :: HSelector (Orderable be) (r (QExpr be s1))
    -> WithExprContext (BeamSqlBackendOrderingSyntax be)
  toVal (HSelector sel) = toOrdering
    p
    (orderByDirection ord)
    (sel $ rewriteThread (Proxy :: Proxy s1) r)

extract
  :: forall r be
   . Proxy be
  -> r
  -> OrderBy (Orderable be) r
  -> WithExprContext (BeamSqlBackendOrderingSyntax be)
extract p r ord = toVal (orderBySelector ord)
 where
  toVal
    :: HSelector (Orderable be) r
    -> WithExprContext (BeamSqlBackendOrderingSyntax be)
  toVal (HSelector sel) = toOrdering p (orderByDirection ord) (sel r)

-- | Order by a list of order parameters.
--
-- Like 'orderBy_', but for "Servant.Crud.OrderBy.OrderBy" instead of tuples.
--
-- A more readable version of the type is, ignoring the nested scopes
--
-- > orderByO_ :: (..) => [OrderBy ..] -> Q be db s (table ..) -> Q be db s (table ..)
orderByO_
  :: forall s table be db
   . Beamable table
  => [OrderBy' be (QNested s) table]
  -> Q be db (QNested s) (table (QExpr be (QNested s)))
  -> Q be db s (table (QExpr be s))
orderByO_ ords2 (Q q) = Q
  (liftF
    (QOrderBy (sequenceA . makeSQLOrdering (Proxy :: Proxy be) ords2)
              q
              (rewriteThread (Proxy :: Proxy s))
    )
  )
 where
  makeSQLOrdering
    :: Proxy be
    -> [OrderBy' be (QNested s) table]
    -> table (QExpr be (QNested s))
    -> [WithExprContext (BeamSqlBackendOrderingSyntax be)]
  makeSQLOrdering p ords tExpr = fmap (extract p tExpr) ords

-- | Adds limit and offsets to restrict to the right page.
--
-- A more readable version of the type is, ignoring the nested scopes
--
-- > setPage :: (..) => Page -> Q be db s a -> Q be db s a
setPage
  :: ( Projectible be a
     , Projectible be (WithRewrittenThread (QNested (QNested s)) (QNested s) a)
     , ThreadRewritable
         (QNested s)
         (WithRewrittenThread (QNested (QNested s)) (QNested s) a)
     , ThreadRewritable (QNested (QNested s)) a
     )
  => Page
  -> Q be db (QNested (QNested s)) a
  -> Q
       be
       db
       s
       ( WithRewrittenThread
           (QNested s)
           s
           (WithRewrittenThread (QNested (QNested s)) (QNested s) a)
       )
setPage (Page (Just off) (Just lim)) = limit_ lim . offset_ off
setPage (Page (Just off) Nothing   ) = offset_ off . offset_ off
setPage (Page Nothing    (Just lim)) = limit_ lim . limit_ lim
setPage (Page Nothing    Nothing   ) = offset_ 0 . offset_ 0

-- | Applies the user requested view to the given query.
--
-- This means that you can include 'View' in the servant api,
-- apply business logic to create a query, and finally use this function to apply
-- the user requested filters, ordering and pageination.
--
-- > type BooksApi = QObj (View Postgres BookT) :> Get ['JSON] [ BookT ]s
-- >
-- > booksServer :: View Postgres BookT -> AppServer [BookT]
-- > booksServer view =
-- >   runDB $ runSelectReturningList $ select $ setView view queryAffectedByBusinessLogic
-- >   where queryAffectedByBusinessLogic = all_ (appDatabase ^. appDatabaseBooks)
--
-- A more readable version of the type is, ignoring the nested scopes
--
-- > setView :: (..) => View .. -> Q be db s (table ..) -> Q be db s (table ..)
setView
  :: ( Beamable table
     , BeamSqlBackend be
     , FieldsFulfillConstraintFilter (Operator Filter be) table
     )
  => View'
       (Orderable be)
       (table (QExpr be (QNested (QNested (QNested s)))))
       (table Filter)
  -> Q
       be
       db
       (QNested (QNested (QNested s)))
       (table (QExpr be (QNested (QNested (QNested s)))))
  -> Q be db s (table (QExpr be s))
setView (View pag ord filt) = setPage pag . orderByO_ ord . matching_ filt


-- | Appies 'setView', then selects and returns the result.
--
-- Useful for GET list requests.
runSetView
  :: ( Beamable table
     , FromBackendRow be (table Identity)
     , FieldsFulfillConstraintFilter (Operator Filter be) table
     , HasQBuilder be
     , MonadBeam be m
     )
  => View'
       (Orderable be)
       (table (QExpr be (QNested (QNested (QNested QBaseScope)))))
       (table Filter)
  -> Q
       be
       db
       (QNested (QNested (QNested QBaseScope)))
       (table (QExpr be (QNested (QNested (QNested QBaseScope)))))
  -> m [table Identity]
runSetView v = runSelectReturningList . select . setView v

runSetViewAround
  :: ( BeamSql2003ExpressionBackend be
     , BeamSql99CommonTableExpressionBackend be
     , MonadBeam be m
     , FromBackendRow be Integer
     , SqlValableTable be (PrimaryKey t)
     , HasTableEquality be (PrimaryKey t)
     , Table t
     , HasQBuilder be
     , FromBackendRow be (t Identity)
     , BeamSqlBackendCanSerialize be Integer
     , FieldsFulfillConstraintFilter (Operator Filter be) t
     )
  => PrimaryKey t Identity
  -> View' (Orderable be) (t (QExpr be s)) (t Filter)
  -> Q be db (QNested QAnyScope) (t (QExpr be (QNested QAnyScope)))
  -> m ([t Identity], Integer, Integer)
runSetViewAround p (View pag ords filt) q = do
  xs <- runSelectReturningList $ selectWith $ do
    numberedRows <- selecting $ withWindow_
      (\i ->
        ( frame_ (noPartition_ @Integer) (noOrder_ @Integer)          noBounds_
        , frame_ (noPartition_ @Integer) (Just $ fmap (mkOrd i) ords) noBounds_
        )
      )
      (\i (win1, win2) -> (i, countAll_ `over_` win1, rowNumber_ `over_` win2))
      (matching_ filt q)
    pure
      $ setPage pag { offset = Nothing }
      $ orderBy_ (\(_, _, rn) -> asc_ rn)
      $ filter_
          (\(_, _, rn) ->
            (rn - fromInteger (fromMaybe 0 (offset pag))) >=. subquery_
              (do
                (x', _, rn') <- reuse numberedRows
                guard_ (primaryKey x' ==. val_ p)
                pure rn'
              )
          )
          (reuse numberedRows)

  let (c, rn) = case xs of
        []               -> (0, 0)
        (_, c', rn') : _ -> (fromInteger c', fromInteger rn')
  pure (fmap (\(x, _, _) -> x) xs, c, rn)
  where mkOrd r ord = QOrd $ extract' Proxy r ord


-- | Aplies the filters from 'View', then counts the total number of rows
runCountInView
  :: ( Beamable table
     , FromBackendRow be Integer
     , FieldsFulfillConstraintFilter (Operator Filter be) table
     , HasQBuilder be
     , MonadBeam be m
     )
  => View'
       (Orderable be)
       (table (QExpr be (QNested (QNested (QNested QBaseScope)))))
       (table Filter)
  -> Q
       be
       db
       (QNested (QNested (QNested QBaseScope)))
       (table (QExpr be (QNested (QNested (QNested QBaseScope)))))
  -> m (Maybe TotalCount)
runCountInView v =
  fmap (fmap TotalCount) . runCountIn . offset_ 0 . offset_ 0 . matching_
    (filters v)

-- | Aplies the filters from 'View', then counts the total number of rows
runPrimaryKeysInView
  :: ( FieldsFulfillConstraintFilter (Operator Filter be) table
     , HasQBuilder be
     , MonadBeam be m
     , Table table
     , FromBackendRow be (PrimaryKey table Identity)
     , SqlValableTable be (PrimaryKey table)
     , HasSqlInTable be
     )
  => table Filter
  -> ExceptLimited [PrimaryKey table Identity]
  -> Q
       be
       db
       (QNested (QNested (QNested QBaseScope)))
       (table (QExpr be (QNested (QNested (QNested QBaseScope)))))
  -> m [PrimaryKey table Identity]
runPrimaryKeysInView fltr ekeys = case ekeys of
  Except    xs -> go not_ xs
  LimitedTo xs -> go id xs
 where
  go fn keys =
    runSelectReturningList
      . select
      . fmap pk
      . offset_ 0
      . offset_ 0
      . offset_ 0
      . filter_ (\t -> fn (pk t `in_` fmap val_ keys))
      . matching_ fltr

-- | As @runInView@ but only returns primary keys and labels
runNamesInViewWithCount
  :: ( FromBackendRow be (table Identity)
     , FieldsFulfillConstraintFilter (Operator Filter be) table
     , HasQBuilder be
     , MonadBeam be m
     , ToName table
     , FromBackendRow be (PrimaryKey table Identity)
     , FromBackendRow be Text
     , BeamSql2003ExpressionBackend be
     , FromBackendRow be Integer
     , Integral c
     )
  => View'
       (Orderable be)
       (table (QExpr be (QNested (QNested (QNested (QNested QBaseScope))))))
       (table Filter)
  -> Q
       be
       db
       (QNested (QNested (QNested (QNested QBaseScope))))
       ( table
           (QExpr be (QNested (QNested (QNested (QNested QBaseScope)))))
       )
  -> m ([Named table Identity], c)
runNamesInViewWithCount v q = do
  (xs, c) <-
    runSelectReturningListWithCount
    . fmap (\x -> (primaryKey x, toName x))
    $ setView v q

  pure (fmap (uncurry Named) xs, c)


runNamesInViewWithCountAround
  :: ( BeamSql2003ExpressionBackend be
     , BeamSql99CommonTableExpressionBackend be
     , FieldsFulfillConstraintFilter (Operator Filter be) t
     , FromBackendRow be (PrimaryKey t Identity)
     , FromBackendRow be Integer
     , FromBackendRow be Text
     , HasQBuilder be
     , HasTableEquality be (PrimaryKey t)
     , MonadBeam be m
     , SqlValableTable be (PrimaryKey t)
     , BeamSqlBackendCanSerialize be Integer
     , Table t
     , ToName t
     )
  => PrimaryKey t Identity
  -> View' (Orderable be) (t (QExpr be s)) (t Filter)
  -> Q be db (QNested QAnyScope) (t (QExpr be (QNested QAnyScope)))
  -> m ([Named t Identity], Integer, Integer)
runNamesInViewWithCountAround p (View pag ords filt) q = do
  xs <- runSelectReturningList $ selectWith $ do
    numberedRows <- selecting $ withWindow_
      (\i ->
        ( frame_ (noPartition_ @Integer) (noOrder_ @Integer)          noBounds_
        , frame_ (noPartition_ @Integer) (Just $ fmap (mkOrd i) ords) noBounds_
        )
      )
      (\i (win1, win2) ->
        ( primaryKey i
        , toName i
        , countAll_ `over_` win1
        , rowNumber_ `over_` win2
        )
      )
      (matching_ filt q)
    pure
      $ setPage pag { offset = Nothing }
      $ orderBy_ (\(_, _, _, rn) -> asc_ rn)
      $ filter_
          (\(_, _, _, rn) ->
            (rn - fromInteger (fromMaybe 0 (offset pag))) >=. subquery_
              (do
                (p', _, _, rn') <- reuse numberedRows
                guard_ (p' ==. val_ p)
                pure rn'
              )
          )
          (reuse numberedRows)

  let (c, rn) = case xs of
        []                  -> (0, 0)
        (_, _, c', rn') : _ -> (fromInteger c', fromInteger (rn' - 1))
  pure (fmap (\(p', x, _, _) -> Named p' x) xs, c, rn)
  where mkOrd r ord = QOrd $ extract' Proxy r ord

