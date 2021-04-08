{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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

import           Servant.Aeson.GenericSpecs     ( apiRoundtripSpecs )
import           Test.Hspec                     ( Spec
                                                , describe
                                                )

import           Database.Beam.TH               ( instancesArbitraryT
                                                , instancesArbitrary
                                                )
import           Database.Beam.Deriving
import           Schema
import           Server                         ( api )

-- | Generate Arbitrary instances
$(instancesArbitrary ''UserT)
$(instancesArbitraryT ''UserRoleTT)

spec :: Spec
spec = describe "Entire Schema" $ apiRoundtripSpecs api
