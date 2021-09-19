{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: SchemaSpec
Description: Tests whether ToJSON and FromJSON match up for the entire api

It may be possible to write instances for FromJSON and ToJSON which do not match.
For example, drop a different number of characters for field names.
This spec tests if all ToJSON and FromJSON instances are valid.
-}
module SchemaSpec
  ( spec
  )
where

import           Prelude

import           Data.Text                      ( Text )
import           Generic.Random
import           Generic.Random.Internal.Generic
import           GHC.Generics                   ( Rep )
import           Servant.Aeson.GenericSpecs     ( apiRoundtripSpecs )
import           Test.Hspec                     ( Spec
                                                , describe
                                                )
import           Test.QuickCheck

import           Api                            ( )
import           Auth
import           Schema
import           Server                         ( api )
import           Types

instance Arbitrary a => Arbitrary (SqlSerial a) where
  arbitrary = SqlSerial <$> arbitrary

instance Arbitrary Role where
  arbitrary = genericArbitraryU

instance Arbitrary Roles where
  arbitrary = Roles <$> arbitrary

instance Arbitrary AuthUser where
  arbitrary = genericArbitraryU

instance (GA UnsizedOpts (Rep (BlogTT f t))) => Arbitrary (BlogTT f t) where
  arbitrary = genericArbitraryU

instance (GA UnsizedOpts (Rep (PrimaryKey (BlogTT f) t))) => Arbitrary (PrimaryKey (BlogTT f) t) where
  arbitrary = genericArbitraryU

instance (Arbitrary (PrimaryKey (BlogTT f) t), Arbitrary (C t Text)) => Arbitrary (Named (BlogTT f) t) where
  arbitrary = Named <$> arbitrary <*> arbitrary

instance (GA UnsizedOpts (Rep (ChannelT t))) => Arbitrary (ChannelT t) where
  arbitrary = genericArbitraryU

instance (GA UnsizedOpts (Rep (PrimaryKey ChannelT t))) => Arbitrary (PrimaryKey ChannelT t) where
  arbitrary = genericArbitraryU

instance (Arbitrary (PrimaryKey ChannelT t), Arbitrary (C t Text)) => Arbitrary (Named ChannelT t) where
  arbitrary = Named <$> arbitrary <*> arbitrary

spec :: Spec
spec = describe "Entire Schema" $ apiRoundtripSpecs api
