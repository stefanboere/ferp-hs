{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Servant.Crud.Server.Deriving
Description: Utilities to automatically derive all kinds of type classes with DerivingVia

This module provides newtypes which can be used to derive instances with the -XDerivingVia
language extension.

-}
module Servant.Crud.Server.Deriving
  ( module Servant.Crud.Deriving
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           Servant.Crud.Deriving
import           Servant.Crud.Server.QueryObject
                                                ( ToParams(..)
                                                , GToParams
                                                , defaultToParams
                                                )

-- | A common type name prefix is dropped
instance (Generic a, Typeable a, GToParams lang ftype (Rep a))
    => ToParams lang ftype (QueryType a) where
  toParams t lang ftype _ = defaultToParams (queryOptions (Proxy :: Proxy a))
                                            t
                                            lang
                                            ftype
                                            (Proxy :: Proxy a)
