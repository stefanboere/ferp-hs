{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-orphans #-}
module Servant.Crud.OrderBySpec
  ( spec
  )
where

import           Prelude

import           Data.Either                    ( isLeft )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Generic.Random                 ( genericArbitraryU )
import           GHC.Generics
import           Servant.API                    ( parseQueryParam
                                                , toQueryParam
                                                )
import           Test.Hspec
import           Test.QuickCheck

import           Servant.Crud.OrderBy

data Rgb = Rgb
    { red :: Int
    , green :: Int
    , blue :: Int
    , alpha :: Int
    }
  deriving (Show, Eq, Generic)

instance Arbitrary Rgb where
  arbitrary = genericArbitraryU

instance Selectors c r => Arbitrary (OrderBy c r) where
  arbitrary = fromSelector <$> elements selectors <*> arbitrary

instance Arbitrary Direction where
  arbitrary = elements [Ascending, Descending]
  shrink Ascending  = [Descending]
  shrink Descending = []

instance Selectors Show Int where
  selectors = leaf

instance Selectors Show Rgb

spec :: Spec
spec = do
  describe "parseQueryParam" $ do
    it "is the inverse of toQueryParam" $ property $ \x ->
      parseQueryParam (toQueryParam x)
        == (Right x :: Either Text (OrderBy Show Rgb))

    it "'+' is also valid in front of the query parameter value"
      $ property
      $ \x ->
          parseQueryParam (addPlus (toQueryParam x))
            == (Right x :: Either Text (OrderBy Show Rgb))

    it "parses a simple example"
      $          fmap (showFieldIn (Rgb 5 7 11 17)) (parseQueryParam "alpha")
      `shouldBe` Right "17"

    it "should not parse a simple example"
      $ fmap (showFieldIn (Rgb 5 7 11 17)) (parseQueryParam "alpha11")
      `shouldSatisfy` isLeft

  describe "toQueryParam"
    $ it "does not contain dots for simple types"
    $ property
    $ \x -> not . Text.any (== '.') $ toQueryParam (x :: OrderBy Show Rgb)

 where
  addPlus :: Text -> Text
  addPlus x = case Text.uncons x of
    Just ('-', _) -> x
    _             -> "+" <> x
