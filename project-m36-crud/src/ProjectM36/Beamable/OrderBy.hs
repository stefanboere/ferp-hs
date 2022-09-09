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

import           Data.Functor.Contravariant     ( contramap )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
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
type OrderBy' t = OrderBy Orderable (TableSettings t)

-- | Expressions which can be turned into order syntax.
--
-- This is exposed to allow the end user to specify more general 'View' types.
class Orderable expr where
  toOrdering :: Direction -> expr -> AttributeOrderExpr

instance Orderable (TableField t a) where
  toOrdering Ascending n =
    AttributeOrderExpr (fieldAttributeName n) AscendingOrder
  toOrdering Descending n =
    AttributeOrderExpr (fieldAttributeName n) DescendingOrder

instance Selectors Orderable (TableField t a) where
  selectors = leaf

type PrimaryKeySelectorsConstraint t s
  = ( Generic (PrimaryKey t (TableField t))
    , GSelectors Orderable (Rep (PrimaryKey t (TableField t)))
    )

instance PrimaryKeySelectorsConstraint t s
    => Selectors Orderable (Named t (TableField t))

instance
    PrimaryKeySelectorsConstraint t s
  => Selectors Orderable (PrimaryKey t (TableField t)) where
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

-- | Order by a list of order parameters.
orderByO_
  :: forall table . (Table table) => [OrderBy' table] -> [AttributeOrderExpr]
orderByO_ = fmap (toOrderExpr (tblFieldSettings :: TableSettings table))
