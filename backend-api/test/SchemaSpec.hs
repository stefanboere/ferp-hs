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

import           Data.Functor.Identity          ( Identity )
import           Generic.Random
import           Generic.Random.Internal.Generic
import           GHC.Generics                   ( Rep )
import           Servant.Aeson.GenericSpecs     ( apiRoundtripSpecs )
import           Test.Hspec                     ( Spec
                                                , describe
                                                )
import           Test.QuickCheck

import           Auth
import           Schema
import           Server                         ( api )


instance Arbitrary User where
  arbitrary = genericArbitraryU

instance Arbitrary Role where
  arbitrary = genericArbitraryU

instance Arbitrary AuthUser where
  arbitrary = genericArbitraryU

instance Arbitrary AuthUserInfo where
  arbitrary = genericArbitraryU

instance (GA UnsizedOpts (Rep (BlogT t))) => Arbitrary (BlogT t) where
  arbitrary = genericArbitraryU

instance Arbitrary (PrimaryKey BlogT Identity) where
  arbitrary = genericArbitraryU

spec :: Spec
spec = describe "Entire Schema" $ apiRoundtripSpecs api
