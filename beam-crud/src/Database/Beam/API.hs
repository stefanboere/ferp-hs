{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , runCountInView
    -- * Advanced
  , Orderable
  , OrderBy'
  , ViewOrderBy
  ) where

import           Prelude

import           Control.Monad.Free             ( liftF )
import           Data.Proxy                     ( Proxy(..) )
import           Database.Beam
import           Database.Beam.Backend.SQL      ( BeamSqlBackend
                                                , BeamSqlBackendOrderingSyntax
                                                )
import           Database.Beam.Expand           ( Named )
import           Database.Beam.Extra            ( runCountIn )
import           Database.Beam.Operator         ( FieldsFulfillConstraintFilter
                                                , Operator
                                                , matching_
                                                )
import           Database.Beam.Query.Internal
import           GHC.Generics                   ( Rep )
import           Servant.API
import           Servant.Crud.API
import           Servant.Crud.Headers           ( PathInfo
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
import           Servant.Crud.QueryOperator     ( Filter
                                                , MaybeLast
                                                )


-- | 'OrderBy' specialized for use with the Beam backend
type OrderBy' be s t = OrderBy (Orderable be) (t (QExpr be s))

-- | @OrderBy'@ with specialized scope
type ViewOrderBy be t
  = OrderBy
      (Orderable be)
      (t (QExpr be (QNested (QNested (QNested QBaseScope)))))

-- | Contains ordering, filtering and pageination info. This particular type works
-- nice with 'setView'. You can use this type with 'QueryObject' to allow the client
-- to specify ordering, filtering and pageination info in the request using request
-- parameters.
--
-- Then, in the handler, simply use 'setView' to alter the query to match this requested view.
type View be t
  = View'
      (Orderable be)
      (t (QExpr be (QNested (QNested (QNested QBaseScope)))))
      (t Filter)

-- | Endpoint returning a list of @t Identity@ based on a @View@ in the request query parameters
--
-- E.g. the type of the @\/books@ endpoint.
type GetList be t = PathInfo :> QObj (View be t) :> GetList' (t Identity)

-- | Regular get requests
type Get_ t = CaptureId t :> Get' (t Identity)

-- | Regular patch requests
type Patch_ t = CaptureId t :> Req' (t MaybeLast) :> Patch_'

-- | Regular put requests
type Put_ t = CaptureId t :> Req' (t Identity) :> Put_'

-- | Regular delete requests
type Delete_ t = CaptureId t :> Delete_'

-- | Delete list requests
type DeleteList_ t = Req' [PrimaryKey t Identity] :> Delete_'

-- | Regular post requests
type Post_ t = PathInfo :> "0" :> Req' (t Identity) :> Post_'

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
