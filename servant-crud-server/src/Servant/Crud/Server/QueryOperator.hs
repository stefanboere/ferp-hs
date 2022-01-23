{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Servant.Crud.Server.QueryOperator
Description: Servant Server specific instances for "Servant.Crud.QueryOperator"

See "Servant.Crud.QueryOperator"
-}
module Servant.Crud.Server.QueryOperator
  ( module Servant.Crud.QueryOperator
  )
where

import           Prelude

import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromMaybe )
import           Data.Swagger.Internal          ( _paramSchemaEnum )
import           Data.Swagger.ParamSchema       ( ToParamSchema(..) )
import qualified Data.Text                     as Text
import           GHC.TypeLits                   ( KnownSymbol
                                                , symbolVal
                                                )
import           Servant
import           Servant.Crud.QueryOperator
import           Servant.Crud.Server.QueryObject
import           Servant.Docs                   ( DocQueryParam(..)
                                                , ToParam(..)
                                                )
import qualified Servant.Docs                  as Docs
                                                ( ParamKind(..) )
import           Servant.Foreign                ( HasForeignType(..) )
import           Test.QuickCheck                ( Arbitrary
                                                , arbitrary
                                                , elements
                                                , shrink
                                                )

instance (HasForeignType lang ftype (Filter a), ToParamSchema a, ToParams lang ftype (FilterT a))
    => ToParams lang ftype (Filter a) where
  toParams t lang ftype _ = [(doc, toParamSchema (Proxy :: Proxy a), qarg)]

   where
    total = toParams t lang ftype (Proxy :: Proxy (FilterT a))
    pdesc (DocQueryParam _ _ x _, _, _) = "\t\t\t" <> x

    desc =
      "It is of the form "
        <> Text.unpack t
        <> "[operator]=value where we have the following options:"

    doc :: DocQueryParam
    doc = DocQueryParam (Text.unpack t)
                        []
                        (unlines (desc : map pdesc total))
                        Docs.Normal

    qarg = typeFor lang ftype (Proxy :: Proxy (Filter a))


instance Arbitrary (FilterT a) => Arbitrary (Filter a) where
  arbitrary = Filter <$> arbitrary
  shrink    = map Filter . shrink . unFilter


-- 'ToParam' instance
instance (KnownSymbol s, KnownParamKind k, ToParamSchema a)
    => ToParam (OpEntry s k a) where
  toParam _ = DocQueryParam (wrap $ symbolVal (Proxy :: Proxy s))
                            (map show enums)
                            ""
                            (toDocParam (paramKindVal (Proxy :: Proxy k)))
   where
    toDocParam :: ParamKind -> Docs.ParamKind
    toDocParam Flag   = Docs.Flag
    toDocParam List   = Docs.List
    toDocParam Normal = Docs.Normal


    enums =
      fromMaybe [] . _paramSchemaEnum . toParamSchema $ (Proxy :: Proxy a)

    wrap :: String -> String
    wrap x = if null x then x else '[' : x ++ "]"


-- 'ToParams' instance
instance ToParams lang ftype (OpDict '[] a) where
  toParams _ _ _ _ = []

instance ( HasForeignType lang ftype a
         , ToParamSchema a
         , KnownSymbol s
         , KnownParamKind k
         , ToParams lang ftype (OpDict xs a)
         )
  => ToParams lang ftype (OpDict ('(s, k) ': xs) a) where
  toParams t lang ftype _ = [(doc, toParamSchema (Proxy :: Proxy a), qarg)]

   where
    qarg = typeFor lang ftype (Proxy :: Proxy a)

    doc :: DocQueryParam
    doc = DocQueryParam
      (Text.unpack t)
      []
      (intercalate ", " (map pname phead ++ map pdesc ptail))
      Docs.Normal

    pname (DocQueryParam x _ _ _, _, _) = x
    pdesc (DocQueryParam _ _ x _, _, _) = x

    phead = toParams t lang ftype (Proxy :: Proxy (OpEntry s k a))
    ptail = toParams t lang ftype (Proxy :: Proxy (OpDict xs a))

instance (KnownSymbol s, KnownParamKind k, ToParamSchema a, HasForeignType lang ftype a)
  => ToParams lang ftype (OpEntry s k a) where
  toParams = toParams1 (Proxy :: Proxy a)

-- 'Arbitrary' instance
instance Arbitrary (OpDict '[] a) where
  arbitrary = pure Nil -- There is nothing to choose from

instance (Arbitrary (OpEntry s k a), Arbitrary (OpDict xs a))
  => Arbitrary (OpDict ('(s, k) ': xs) a) where
  arbitrary = (:>) <$> arbitrary <*> arbitrary

instance (KnownSymbol s, KnownParamKind k, Arbitrary (ParamKindFunctor k a))
  => Arbitrary (OpEntry s k a) where
  arbitrary = E <$> arbitrary

-- Tagged bool Arbitrary
instance Arbitrary (TaggedBool a) where
  arbitrary = elements [NotPresent, Present]
  shrink Present    = [NotPresent]
  shrink NotPresent = []
