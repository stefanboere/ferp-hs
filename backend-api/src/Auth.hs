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
  ( authCheck
  , AuthUser(..)
  , Auth.Auth
  , Admin
  , AdminOrExtra
  , InRoles
  , Everyone
  , AuthApi
  , authServer
  )
where


import           Control.Monad.Reader           ( asks )
import           Crypto.PasswordStore
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BL
import           Data.Int                       ( Int64 )
import           Data.Maybe                     ( mapMaybe )
import           Data.Pool                      ( Pool
                                                , withResource
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Database.Beam
import           Database.Beam.Postgres         ( Connection
                                                , Pg
                                                , runBeamPostgres
                                                )
import           GHC.Generics                   ( Generic )

import           Schema
import           Servant                       as S
import           Servant.AccessControl          ( Auth'
                                                , Everyone
                                                , HasAccessControl(..)
                                                )
import qualified Servant.Auth                  as SA
                                                ( BasicAuth
                                                , Cookie
                                                , JWT
                                                )
import           Servant.Auth.Server           as SAS

import           Context                        ( AppServer
                                                , getJwtSettings
                                                )


-- | Add this to the types to protect it with login
-- > type ProtectedApi = Auth :> Api
type Auth = Auth' '[SA.BasicAuth, SA.Cookie, SA.JWT] AuthUser

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

-- | How we represent a user.
data AuthUser = AuthUser
  { getUserId    :: Int64
  , getUserRoles :: [Role]
  }
  deriving (Show, Generic)

instance ToJSON AuthUser where
  toJSON (AuthUser userid userroles) =
    object ["userId" .= userid, "userRoles" .= userroles]

instance FromJSON AuthUser where
  parseJSON = withObject "AuthUser"
    $ \v -> AuthUser <$> v .: "userId" <*> v .: "userRoles"

instance ToJWT AuthUser
instance FromJWT AuthUser


-- | Checks if a BasicAuthData is a valid user
authCheck :: Pool Connection -> BasicAuthData -> IO (AuthResult AuthUser)
authCheck conns (BasicAuthData login password) = do
  mUser <- withResource conns $ \conn -> runBeamPostgres conn userQuery

  pure $ case mUser of
    [] -> SAS.NoSuchUser
    (user, _) : _ ->
      if verifyPassword password (Text.encodeUtf8 (userPassword user))
        then SAS.Authenticated (AuthUser (userId user) (mapMaybe snd mUser))
        else SAS.BadPassword

 where
  userQuery :: Pg [(User, Maybe Role)]
  userQuery = runSelectReturningList $ select $ do
    user <- all_ (_appDatabaseUsers appDatabase)
    role <- leftJoin_ (all_ (_appDatabaseUserRoles appDatabase))
                      (\role -> userRoleUser role `references_` user)
    guard_ (userEmail user ==. val_ (Text.toLower . Text.decodeUtf8 $ login))
    pure (user, userRoleRole role)


type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthUser)

instance FromBasicAuthData AuthUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData


-- | A user together with a JWT token
data AuthUserInfo = AuthUserInfo
  { authUser :: AuthUser
  , token    :: Text
  }
  deriving Generic

instance ToJSON AuthUserInfo

-- | Endpoint with data about the logged in user
type AuthApi = Auth.Auth Everyone :> "self" :> Get '[JSON] AuthUserInfo

authServer :: AppServer AuthApi
authServer u = do
  jwtSettings <- asks getJwtSettings
  ejwt        <- liftIO $ makeJWT u jwtSettings Nothing
  case ejwt of
    Right jwt -> pure $ AuthUserInfo u (Text.decodeUtf8 . BL.toStrict $ jwt)
    Left  _   -> throwError $ err500 { errBody = "Internal server error" }
