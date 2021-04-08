{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.Crud.QueryObjectSpec
  ( spec
  )
where

import           Prelude

import           Data.Proxy
import           Generic.Random                 ( genericArbitraryU )
import           GHC.Generics
import           Test.Hspec
import           Test.QuickCheck

import           Servant.Crud.QueryObject

-- | Combined spec for query objects
queryObjectSpec
  :: (FromQueryText a, ToQueryText a, Show a, Eq a, Arbitrary a)
  => Proxy a
  -> Spec
queryObjectSpec p = do
  roundTripSpec p
  emptyParamSpec p
  emptyQuerySpec p

-- Tests whether toQueryText and then fromQueryText is the identity
roundTripSpec
  :: (FromQueryText a, ToQueryText a, Show a, Eq a, Arbitrary a)
  => Proxy a
  -> Spec
roundTripSpec p =
  describe "fromQueryText"
    $ it "should be the inverse of toQueryText"
    $ property
    $ \x -> (actuallyFound . fromQueryText "" . toQueryText "") x
        == Found (x `asProxyTypeOf` p)

-- Tests whether the result of toQueryText does not contain empty parameters
emptyParamSpec :: (ToQueryText a, Show a, Arbitrary a) => Proxy a -> Spec
emptyParamSpec p =
  describe "toQueryText"
    $ it "should not result in any empty parameters"
    $ property
    $ \x -> not . any ((== "") . fst) $ toQueryText "" (x `asProxyTypeOf` p)

-- | Tests whether there is no error on parsing the empty list
emptyQuerySpec :: (FromQueryText a, Show a) => Proxy a -> Spec
emptyQuerySpec p =
  describe "fromQueryText"
    $               it "should parse an empty query without errors"
    $               ((`asProxyTypeOf` p) <$> fromQueryText "" [])
    `shouldSatisfy` isAbsent


data Color = Blue | Green | Red
  deriving (Show, Eq, Generic, FromQueryText, ToQueryText)

data Unit = Unit
  deriving (Show, Eq, Generic, FromQueryText, ToQueryText)

newtype Singleton = Singleton { single :: Bool }
  deriving (Show, Eq, Generic, FromQueryText, ToQueryText)

data Rgb = Rgb
    { red :: Maybe Int
    , green :: Maybe Int
    , blue :: Maybe Int
    , alpha :: Alpha
    }
  deriving (Show, Eq, Generic, FromQueryText, ToQueryText)

data Alpha = Alpha
  { transparency :: Maybe Int
  , alpha2 :: Maybe Int
  }
  deriving (Show, Eq, Generic, FromQueryText, ToQueryText)

data NamedColor =
    Custom Color
  | Regular Color
    deriving (Show, Eq, Generic, FromQueryText, ToQueryText)

instance Arbitrary NamedColor where
  arbitrary = genericArbitraryU

instance Arbitrary Rgb where
  arbitrary = genericArbitraryU

instance Arbitrary Alpha where
  arbitrary = genericArbitraryU

instance Arbitrary Color where
  arbitrary = genericArbitraryU

instance Arbitrary Singleton where
  arbitrary = genericArbitraryU

spec :: Spec
spec = do
  queryObjectSpec (Proxy :: Proxy Color)
  queryObjectSpec (Proxy :: Proxy NamedColor)
  queryObjectSpec (Proxy :: Proxy Rgb)
  queryObjectSpec (Proxy :: Proxy Singleton)

  describe "fromQueryText" $ do
    it "should be case invariant"
      $          fromQueryText "" [("sInglE", Nothing)]
      `shouldBe` Found (Singleton True)

    it "should read values while respecting casing"
      $          fromQueryText "" [("custom", Nothing)]
      `shouldBe` Absent (Custom Blue)

    it "should handle defaults"
      $          fromQueryText "absentkey" []
      `shouldBe` (Absent Nothing :: ParseResult (Maybe Int))

    it "should handle defaults"
      $          fromQueryText "absentkey" []
      `shouldBe` (Absent Blue :: ParseResult Color)

  describe "toQueryText" $ do
    it "should print names as lowercase"
      $          toQueryText "" (Singleton True)
      `shouldBe` [("single", Nothing)]

    it "should prefix when asked"
      $          toQueryText "prefix" Unit
      `shouldBe` [("prefix", Nothing)]

    it "should print properly"
      $          toQueryText "" (Custom Blue)
      `shouldBe` [("custom.blue", Nothing)]

    it "should put dots as separator"
      $ toQueryText "" (Rgb Nothing Nothing (Just 2) (Alpha (Just 3) (Just 4)))
      `shouldBe` [ ("blue"              , Just "2")
                 , ("alpha.transparency", Just "3")
                 , ("alpha.alpha2"      , Just "4")
                 ]
