{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}
{-|
Module: ApiSpec
Description: Tests the business logic of the api.

This is a shared spec for all api endpoints for two reasons.
 * Creating a client is a resource intensive process
 * The business logic does not restrict to one endpoint. You need to test how the
   endpoints relate.
-}
module ApiSpec
  ( spec
  )
where

import           Prelude

import           Data.Default                   ( def )
import           Data.Either                    ( isRight )
import           Network.HTTP.Client            ( Manager
                                                , defaultManagerSettings
                                                , newManager
                                                )
import           Servant
import           Servant.Client
import           Servant.Client.Generic         ( AsClientT
                                                , genericClient
                                                )
import           Servant.Query                  ( CrudRoutes
                                                , _getList
                                                )
import           Test.Hspec

import           Schema
import           Server                         ( withTestApplication )

type Port = Int

-- TODO read this from the environment
cred :: BasicAuthData
cred = BasicAuthData "user" "pass"

-- | Runs the client
runClient :: Manager -> Port -> ClientM a -> IO (Either ClientError a)
runClient mananger port f = do
  let url       = BaseUrl Http "localhost" port ""
  let clientEnv = mkClientEnv mananger url
  runClientM f clientEnv


getUsers :: ClientM [User]
getUsers = getResponse <$> _getList userRoutes def


userRoutes :: CrudRoutes UserT UserT (AsClientT ClientM)
userRoutes = genericClient

-- | Test spec for business logic
spec :: Spec
spec = around withTestApplication $ do
  -- Get the client manager
  m <- runIO $ newManager defaultManagerSettings
  -- The testing specs
  describe "GET /users" $ it "should not result in an error" $ \p -> do
    result <- runClient m p getUsers
    result `shouldSatisfy` isRight
