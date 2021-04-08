{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Servant.Crud.Server.OrderBy
Description: Orphan instances for "Servant.Crud.OrderBy"
-}
module Servant.Crud.Server.OrderBy
  ( module Servant.Crud.OrderBy
  )
where

import           Prelude

import           Data.Proxy                     ( Proxy(..) )
import           Data.Swagger.ParamSchema       ( ToParamSchema(..) )
import qualified Data.Text                     as Text
import           GHC.TypeLits                   ( KnownSymbol
                                                , symbolVal
                                                )
import           Servant                        ( QueryParams )
import           Servant.Crud.OrderBy
import           Servant.Docs                   ( DocQueryParam(..)
                                                , ParamKind(..)
                                                , ToParam(..)
                                                )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , elements
                                                )


instance Selectors c r => Arbitrary (OrderBy c r) where
  arbitrary = fromSelector <$> elements selectors <*> arbitrary

instance Arbitrary Direction where
  arbitrary = elements [Ascending, Descending]
  shrink Ascending  = [Descending]
  shrink Descending = []

instance ToParamSchema (OrderBy c r) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy [Text.Text]) -- TODO

-- Param instances
instance (KnownSymbol s, Selectors c r) => ToParam (QueryParams s (OrderBy c r)) where
  toParam _ = DocQueryParam (symbolVal (Proxy :: Proxy s))
                            (values ++ fmap ("-" <>) values)
                            desc
                            List
   where
    values = fmap (Text.unpack . Text.intercalate "." . fst) sels

    sels :: [(Path, HSelector c r)]
    sels = selectors

    desc = unlines
      [ "Specifies how the result is ordered. "
      , "The '-' in front of the column name indicates 'reverse the ordering to highest first'."
      ]
