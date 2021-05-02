{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module: Auth
Description: Implements the basic auth check
-}
module Auth
  ( AuthUser(..)
  , Role(..)
  , Roles(..)
  , Auth.Auth
  , Admin
  , AdminOrExtra
  , InRoles
  , Everyone
  , AuthApi
  , authServer
  , getUserRoles
  )
where

import qualified Crypto.JWT                    as Jose
import           Data.Aeson
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Exts                       ( toList )
import           GHC.Generics                   ( Generic )
import           Lens.Micro                     ( (^.) )
import           Servant                       as S
import           Servant.AccessControl.Server   ( Auth'
                                                , Everyone
                                                , HasAccessControl(..)
                                                )
import qualified Servant.Auth                  as SA
                                                ( Cookie
                                                , JWT
                                                )
import           Servant.Auth.Server           as SAS
import           Text.Read                      ( readMaybe )

import           Context                        ( AppServer )


-- | Add this to the types to protect it with login
-- > type ProtectedApi = Auth :> Api
type Auth = Auth' '[SA.Cookie, SA.JWT] AuthUser

-- brittany-disable-next-binding
data InRoles (roles :: [Role])

-- brittany-disable-next-binding
type Admin = InRoles '[ 'Administrator]
type AdminOrExtra = InRoles '[ 'Administrator, 'Extra]

instance HasAccessControl AuthUser (InRoles '[]) where
  hasAccess _ _ = False

instance (KnownRole x, HasAccessControl AuthUser (InRoles xs))
    => HasAccessControl AuthUser (InRoles (x ': xs)) where
  hasAccess _ u =
    roleVal (Proxy :: Proxy x)
      `elem` getUserRoles u
      ||     hasAccess (Proxy :: Proxy (InRoles xs)) u


-- * Auth user

getUserRoles :: AuthUser -> [Role]
getUserRoles = unRoles . authRealmAccess

-- | User information stored in the access token
data AuthUser = AuthUser
  { authName              :: Text
  , authPreferredUsername :: Text
  , authEmail             :: Text
  , authEmailVerified     :: Bool
  , authRealmAccess       :: Roles
  }
  deriving (Eq, Show, Generic)

aesonOpts :: Options
aesonOpts = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance FromJSON AuthUser where
  parseJSON = genericParseJSON aesonOpts

instance ToJSON AuthUser where
  toJSON = genericToJSON aesonOpts

instance FromJWT AuthUser where
  decodeJWT m = case fromJSON (Object (m ^. Jose.unregisteredClaims)) of
    Error   e -> Left $ Text.pack e
    Success a -> Right a

instance ToJWT AuthUser where
  encodeJWT m = case toJSON m of
    Object x ->
      foldr (\(a, b) c -> Jose.addClaim a b c) Jose.emptyClaimsSet (toList x)
    _ -> Jose.emptyClaimsSet

newtype Roles = Roles { unRoles :: [ Role ] } deriving (Eq, Show)

instance FromJSON Roles where
  parseJSON x = do
    xs <- withObject "Roles" (.: "roles") x
    pure $ Roles $ mapMaybe (readMaybe . Text.unpack . Text.toTitle) xs

instance ToJSON Roles where
  toJSON (Roles xs) = object ["roles" .= toJSON xs]


data Role = Regular | Extra | Administrator
  deriving (Show, Read, Enum, Bounded, Eq, Ord, Generic)

instance ToJSON Role

class KnownRole (r :: Role) where
  roleVal :: Proxy r -> Role

instance KnownRole 'Administrator where
  roleVal _ = Administrator
instance KnownRole 'Extra where
  roleVal _ = Extra
instance KnownRole 'Regular where
  roleVal _ = Regular

-- | Endpoint with data about the logged in user
type AuthApi = Auth.Auth Everyone :> "self" :> Get '[JSON] AuthUser

authServer :: AppServer AuthApi
authServer = pure

