{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Servant.Crud.QueryOperatorSpec
  ( spec
  )
where

import           Prelude

import           Data.Default                   ( def )
import           Data.String                    ( fromString )
import qualified Data.Text                     as Text
import           GHC.TypeLits                   ( KnownSymbol )
import           Test.Hspec
import           Test.QuickCheck

import           Servant.Crud.QueryOperator
import           Servant.Crud.QueryObject

type instance DefaultFilters String = StrFilter
type instance DefaultFilters Int = OrdFilter
type instance DefaultFilters Bool = EqFilter
type instance DefaultFilters (Maybe a) = AddNullFilter (DefaultFilters a)

instance (KnownSymbol s, KnownParamKind k, Arbitrary (ParamKindFunctor k a))
  => Arbitrary (OpEntry s k a) where
  arbitrary = E <$> arbitrary

instance Arbitrary Text.Text where
  arbitrary = Text.pack <$> arbitrary
  shrink xs = Text.pack <$> shrink (Text.unpack xs)

spec :: Spec
spec = do
  describe "toQueryText" $ do
    it "should result in an empty list for empty filters"
      $          toQueryText "id" (def :: Filter String)
      `shouldBe` []

    it "should not be empty for nonempty filters" $ property $ \x ->
      x /= def ==> not . null $ toQueryText
        "id"
        (E x :: OpEntry "ge" 'Normal String)

    it "should be equal on singleton dictionary as on the item itself"
      $ property
      $ \x -> toQueryText "id" (x :> Nil)
          == toQueryText "id" (x :: OpEntry "ge" 'Normal String)

    it "should print Entry types properly" $ property $ \x ->
      (not . null $ x)
        ==>        toQueryText
                     "id"
                     (E (Last $ Just (fromString x)) :: OpEntry "ge" 'Normal String)
        `shouldBe` [("id[ge]", Just (fromString x))]

    it "should hide brackets for empty symbols" $ property $ \x ->
      toQueryText
          "id"
          (E (Last $ Just (fromString x)) :: OpEntry "" 'Normal String)
        `shouldBe` [("id", Just (fromString x))]

  describe "fromQueryText" $ do
    it "should parse an empty query parameter as nothing"
      $          fromQueryText "id" []
      `shouldBe` Absent (def :: Filter String)

    it "should parse [] as an empty query operator"
      $          fromQueryText "xs" [("xs[]", Just "foo")]
      `shouldBe` (fromQueryText "xs" [("xs", Just "foo")] :: ( ParseResult
                       (OpEntry "" 'Normal String)
                   )
                 )

    it
        "should parse a nonempty query paramter as not nothing if the name matches"
      $ property
      $ \x ->
          fromQueryText "id" [("id[ge]", Just x)]
            `notElem` ([Found def, Absent def] :: [ ParseResult
                            (OpEntry "ge" 'Normal String)
                        ]
                      )

    it "should work parse Entry types properly"
      $          fromQueryText "id" [("id[ge]", Just "1")]
      `shouldBe` Found (E (Last $ Just 1) :: OpEntry "ge" 'Normal Int)
