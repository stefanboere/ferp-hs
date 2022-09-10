{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module ProjectM36.Beamable.OrderBy
  ( -- * Ordering
    orderByO_
  , OrderBy'
  , BeamOrderBy(..)
  , Orderable
  ) where

import           Data.Functor.Const             ( Const(..) )
import           Data.Functor.Contravariant     ( contramap )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           ProjectM36.Base                ( AttributeName )
import           ProjectM36.Beamable.Class
import           ProjectM36.Beamable.Expand
import           ProjectM36.DataFrame           ( AttributeOrderExpr(..)
                                                , Order(..)
                                                )
import           Servant.Crud.Deriving          ( queryOptions )
import           Servant.Crud.OrderBy
import           Servant.Crud.QueryObject       ( Options(..)
                                                , defaultOptions
                                                )

-- | Newtype for usage with -XDeriveVia to derive instances 'Selectors', using
-- 'queryOptions'  instead of the default options.
newtype BeamOrderBy a = BeamOrderBy { unBeamOrderBy :: a }

-- | A common type name prefix is dropped
instance ( Typeable r, Generic r, GSelectors Orderable (Rep r))
      => Selectors Orderable (BeamOrderBy r) where
  selectors = fmap (contramap unBeamOrderBy)
    <$> defaultSelectors (queryOptions (Proxy :: Proxy r))


-- | 'OrderBy' specialized for use with the ProjectM36 backend
type OrderBy' t = OrderBy Orderable (t (Const AttributeName))

-- | Expressions which can be turned into order syntax.
--
-- This is exposed to allow the end user to specify more general 'View' types.
class Orderable expr where
  toOrdering :: Direction -> expr -> AttributeOrderExpr

instance Orderable (Const AttributeName t) where
  toOrdering Ascending  (Const n) = AttributeOrderExpr n AscendingOrder
  toOrdering Descending (Const n) = AttributeOrderExpr n DescendingOrder

instance Selectors Orderable (Const AttributeName a) where
  selectors = leaf

type PrimaryKeySelectorsConstraint t
  = ( Generic (PrimaryKey t (Const AttributeName))
    , GSelectors Orderable (Rep (PrimaryKey t (Const AttributeName)))
    )

instance PrimaryKeySelectorsConstraint t
    => Selectors Orderable (Named t (Const AttributeName))

instance
    PrimaryKeySelectorsConstraint t
  => Selectors Orderable (PrimaryKey t (Const AttributeName)) where
  selectors = defaultSelectors
    (defaultOptions { fieldLabelModifier     = const ""
                    , constructorTagModifier = const ""
                    }
    )

toOrderExpr :: forall r . r -> OrderBy Orderable r -> AttributeOrderExpr
toOrderExpr r ord = toVal (orderBySelector ord)
 where
  toVal :: HSelector Orderable r -> AttributeOrderExpr
  toVal (HSelector sel) = toOrdering (orderByDirection ord) (sel r)

tblAttributeNames :: forall t . Table t => t (Const AttributeName)
tblAttributeNames = runIdentity
  $ zipBeamFieldsM go (tblFieldSettings :: TableSettings t) tblSkeleton
 where
  go
    :: Columnar' (TableField t) a
    -> Columnar' Proxy a
    -> Identity (Columnar' (Const AttributeName) a)
  go (Columnar' x) _ = Identity $ Columnar' $ Const $ fieldAttributeName x

-- | Order by a list of order parameters.
orderByO_
  :: forall table . (Table table) => [OrderBy' table] -> [AttributeOrderExpr]
orderByO_ =
  fmap (toOrderExpr (tblAttributeNames :: table (Const AttributeName)))
