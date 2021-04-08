{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ServerSpec
  ( spec
  , quickcheckSpec
  , waiSpec
  )
where

import           Prelude

import           Servant.QuickCheck
import           Test.Hspec
import           Test.Hspec.Wai

import           SchemaSpec                     ( )
import           Server                         ( api
                                                , testApplication
                                                , withTestApplication
                                                )

args :: Args
args = defaultArgs { maxSuccess = 500 }

spec :: Spec
spec = do
  quickcheckSpec
  waiSpec


-- | This allows you to test global properties of the endpoints
quickcheckSpec :: Spec
quickcheckSpec =
  describe "Api"
    $ it "should demonstrate best practices"
    $ withTestApplication
    $ \port -> serverSatisfies
        api
        (BaseUrl Http "localhost" port "")
        args
        (   not500
        <%> onlyJsonObjects
        <%> honoursAcceptHeader
        <%> notAllowedContainsAllowHeader
        <%> unauthorizedContainsWWWAuthenticate
        <%> getsHaveLastModifiedHeader
        <%> getsHaveCacheControlHeader
        <%> headsHaveCacheControlHeader
        <%> createContainsValidLocation
        <%> mempty
        )

-- | This allows you to make arbitrary requests and see what to expect
waiSpec :: Spec
waiSpec =
  with testApplication
    $                   describe "GET /non-existent"
    $                   it "responds with 404"
    $                   get "/non-existent"
    `shouldRespondWith` 404
