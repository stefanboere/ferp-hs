{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module OIDC
  ( OIDCConfig(..)
  , OIDCEnv(..)
  , IdentityApi
  , handleLogin
  , handleLoginSuccess
  , handleLoginFailed
  , initOIDC
  )
where



import           Control.Monad.Except
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Builder        ( toLazyByteString )
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as BL
                                                ( toStrict )
import           Data.IORef                     ( IORef
                                                , atomicModifyIORef'
                                                , newIORef
                                                , readIORef
                                                )
import qualified Data.List                     as List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding            as Text
import           Dhall                          ( Generic
                                                , Interpret(..)
                                                )
import           Network.HTTP.Client            ( Manager
                                                , newManager
                                                )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import qualified Network.URI                   as Network
import           Servant
import qualified System.Random                 as Random
import           Web.Cookie                     ( SetCookie(..)
                                                , defaultSetCookie
                                                , parseCookies
                                                , renderSetCookie
                                                )
import qualified Web.OIDC.Client               as O


-- | Command line options.
data OIDCConfig = OIDCConfig
  { oidcProviderUri  :: URI -- Provider Uri, without `.well-known/openid-configuration`
  , oidcRedirectUri  :: URI  -- Should have /success and /error endpoints
  , oidcClientSecret :: ClientSecret
  , oidcClientId     :: Text
  }
  deriving Generic

-- | A newtype wrapper around 'BytesString' that does not have a Show instance
newtype ClientSecret = ClientSecret { unClientSecret :: ByteString }

instance Interpret ClientSecret where
  autoWith cfg = ClientSecret . Text.encodeUtf8 <$> autoWith cfg


instance Interpret Network.URI where
  autoWith cfg = go <$> autoWith cfg
   where
    go :: Text -> Network.URI
    go = fromMaybe (error "") . Network.parseURI . Text.unpack

instance Interpret OIDCConfig

data OIDCEnv = OIDCEnv
  { oidcCredentials :: O.OIDC
  , oidcManager     :: Manager
  , oidcGenState    :: IO ByteString
  , oidcState       :: IORef SessionStateMap
  }

type SessionStateMap = Map Text (O.State, O.Nonce)

initOIDC :: OIDCConfig -> IO OIDCEnv
initOIDC OIDCConfig {..} = do
  ssm  <- newIORef mempty
  mgr  <- newManager tlsManagerSettings
  prov <- O.discover (Text.pack . show $ oidcProviderUri) mgr
  let oidc = O.setCredentials (Text.encodeUtf8 oidcClientId)
                              (unClientSecret oidcClientSecret)
                              (B.pack . show $ oidcRedirectUri)
                              (O.newOIDC prov)
  return OIDCEnv { oidcCredentials = oidc
                 , oidcManager     = mgr
                 , oidcGenState    = genRandomBS
                 , oidcState       = ssm
                 }


-- brittany-disable-next-binding
type Login = "login" :> Get '[JSON] NoContent

-- brittany-disable-next-binding
type LoginSuccess = "return"
  :> QueryParam' '[Required] "code"  Text
  :> QueryParam' '[Required] "state"  Text
  :> Header "cookie" SessionCookie
  :> Get '[JSON] User

-- brittany-disable-next-binding
type LoginError = "return"
  :> QueryParam' '[Required] "error" Text
  :> Get '[JSON] User

-- brittany-disable-next-binding
type IdentityApi = Login :<|> LoginSuccess :<|> LoginError

data Message = Message
  { message :: Text
  , title   :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Message

newtype SessionCookie = SessionCookie
  { getSessionCookie :: ByteString
  }

instance FromHttpApiData SessionCookie where
  parseUrlPiece = parseHeader . Text.encodeUtf8
  parseHeader bs = case lookup "session" (parseCookies bs) of
    Nothing  -> Left "session cookie missing"
    Just val -> Right (SessionCookie val)

-- Redirect the user to the provider.
handleLogin
  :: (MonadError ServerError m, MonadIO m) => OIDCEnv -> ServerT Login m
handleLogin oidcEnv = do
  (loc, sid) <- liftIO (genOIDCURL oidcEnv)
  let cookie =
        defaultSetCookie { setCookieName = "session", setCookieValue = sid }
  throwError err302
    { errHeaders = [ ("Location", loc)
                   , ( "Set-Cookie"
                     , BL.toStrict (toLazyByteString (renderSetCookie cookie))
                     )
                   ]
    }

genOIDCURL :: OIDCEnv -> IO (ByteString, ByteString)
genOIDCURL env@OIDCEnv {..} = do
  sid <- oidcGenState
  let store = sessionStore env (Text.decodeUtf8 sid)
  loc <- O.prepareAuthenticationRequestUrl
    store
    oidcCredentials
    [O.openId, O.email, O.profile, "roles"]
    []
  return (B.pack $ show loc, sid)

sessionStore :: OIDCEnv -> Text -> O.SessionStore IO
sessionStore OIDCEnv {..} sid' = O.SessionStore
  { sessionStoreGenerate = oidcGenState
  , sessionStoreSave     = saveState oidcState sid'
  , sessionStoreGet      = getStateBy oidcState sid'
  , sessionStoreDelete   = deleteState oidcState sid'
  }

 where
  saveState ssm sid st nonce =
    liftIO $ atomicModifyIORef' ssm $ \m -> (Map.insert sid (st, nonce) m, ())
  getStateBy ssm sid = liftIO $ do
    m <- Map.lookup sid <$> readIORef ssm
    pure $ case m of
      Just (st, nonce) -> (Just st, Just nonce)
      _                -> (Nothing, Nothing)
  deleteState ssm sid =
    liftIO $ atomicModifyIORef' ssm $ \m -> (Map.delete sid m, ())


-- | @AuthInfo@
data AuthInfo = AuthInfo
  { email             :: Maybe Text
  , emailVerified     :: Bool
  , name              :: Maybe Text
  , preferredUsername :: Maybe Text
  }
  deriving (Eq, Show, Generic)

aesonCamel :: Aeson.Options
aesonCamel =
  Aeson.defaultOptions { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' }

instance FromJSON AuthInfo where
  parseJSON = Aeson.genericParseJSON aesonCamel

instance ToJSON AuthInfo where
  toJSON = Aeson.genericToJSON aesonCamel

data User = User
  { userEmail        :: Maybe Text
  , userFullname     :: Maybe Text
  , userUsername     :: Maybe Text
  , userAccessToken  :: Text
  , userRefreshToken :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON User

handleLoginSuccess
  :: (MonadError ServerError m, MonadIO m)
  => OIDCEnv
  -> Text -- ^ code
  -> Text -- ^ state
  -> Maybe SessionCookie
  -> m User
handleLoginSuccess oidcenv oauthCode state (Just cookie) = do
  let sid   = Text.decodeUtf8 $ getSessionCookie cookie
  let store = sessionStore oidcenv sid
  tokens <- liftIO $ O.getValidTokens store
                                      (oidcCredentials oidcenv)
                                      (oidcManager oidcenv)
                                      (Text.encodeUtf8 state)
                                      (Text.encodeUtf8 oauthCode)
  let idToken  = O.idToken . setType $ tokens
  let authInfo = O.otherClaims idToken
  if emailVerified authInfo
    then do
      pure $ User { userEmail        = email authInfo
                  , userFullname     = name authInfo
                  , userUsername     = preferredUsername authInfo
                  , userAccessToken  = O.accessToken tokens
                  , userRefreshToken = O.refreshToken tokens
                  }
    else forbidden "Please verify your email"
 where
  setType :: O.Tokens AuthInfo -> O.Tokens AuthInfo
  setType = id

handleLoginSuccess _ _ _ Nothing = forbidden "Session cookie missing"

handleLoginFailed
  :: (MonadError ServerError m)
  => Text -- ^ error
  -> m User
handleLoginFailed = forbidden

appToErr :: ServerError -> Text -> ServerError
appToErr x msg = x
  { errBody    = Aeson.encode (Message msg (Text.pack $ errReasonPhrase x))
  , errHeaders = [("Content-Type", "text/json")]
  }

forbidden :: (MonadError ServerError m) => Text -> m a
forbidden = throwError . forbiddenErr

forbiddenErr :: Text -> ServerError
forbiddenErr = appToErr err403

genRandomBS :: IO ByteString
genRandomBS = do
  g <- Random.newStdGen
  pure $ B.pack $ readable 0 $ fmap toChar $ take 42 $ Random.randomRs (0, n) g
 where
  n = length letters - 1
  toChar i = letters List.!! i
  letters = ['A' .. 'Z'] <> ['0' .. '9'] <> ['a' .. 'z']
  readable :: Int -> [Char] -> [Char]
  readable _ [] = []
  readable i str =
    let blocksize = case n of
          0 -> 8
          1 -> 4
          2 -> 4
          3 -> 4
          _ -> 12
        block = take blocksize str
        rest  = drop blocksize str
    in  if List.null rest then str else block <> "-" <> readable (i + 1) rest
