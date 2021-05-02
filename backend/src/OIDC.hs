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
  , genOIDCURL
  , handleLoginSuccess
  , handleLoginFailed
  , initOIDC
  , setCookieBs
  -- * JWT
  , discoverJWKs
  , generateJwtSettings
  )
where



import           Control.Monad.Except
import qualified Crypto.JOSE                   as Jose
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
import           Data.String                    ( IsString(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding            as Text
import           Dhall                          ( Generic
                                                , Interpret(..)
                                                )
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import qualified Network.URI                   as Network
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.Internal.Cookie
import qualified System.Random                 as Random
import           Web.Cookie                     ( SetCookie(..)
                                                , defaultSetCookie
                                                , parseCookieExpires
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
  { oidcCredentials    :: O.OIDC
  , oidcManager        :: Manager
  , oidcGenState       :: IO ByteString
  , oidcState          :: IORef SessionStateMap
  , oidcCookieSettings :: CookieSettings
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
  return OIDCEnv
    { oidcCredentials    = oidc
    , oidcManager        = mgr
    , oidcGenState       = genRandomBS
    , oidcState          = ssm
    , oidcCookieSettings = defaultCookieSettings
                             { cookieXsrfSetting = Just $ def
                                                     { xsrfExcludeGet = True
                                                     }
                             , cookieIsSecure    = NotSecure
                             } -- cookie config
    }


-- brittany-disable-next-binding
type LoginSuccess = "return"
  :> QueryParam' '[Required] "code"  Text
  :> QueryParam' '[Required] "state"  Text
  :> Header "cookie" SessionCookie
  :> Header "cookie" RedirectCookie
  :> Get '[JSON] NoContent

-- brittany-disable-next-binding
type LoginError = "return"
  :> QueryParam' '[Required] "error" Text
  :> Get '[JSON] NoContent

-- brittany-disable-next-binding
type IdentityApi = LoginSuccess :<|> LoginError

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
  parseHeader bs = case lookup "oidc-session" (parseCookies bs) of
    Nothing  -> Left "oidc-session cookie missing"
    Just val -> Right (SessionCookie val)

-- | Route to redirect to on succesful login
newtype RedirectCookie = RedirectCookie
  { getRedirectCookie :: ByteString
  }

instance FromHttpApiData RedirectCookie where
  parseUrlPiece = parseHeader . Text.encodeUtf8
  parseHeader bs = case lookup "redirect" (parseCookies bs) of
    Nothing  -> Right (RedirectCookie "/")
    Just val -> Right (RedirectCookie val)

genOIDCURL :: OIDCEnv -> IO (ByteString, ByteString)
genOIDCURL env@OIDCEnv {..} = do
  sid <- oidcGenState
  let store = sessionStore env (Text.decodeUtf8 sid)
  loc <- O.prepareAuthenticationRequestUrl store
                                           oidcCredentials
                                           [O.openId, O.email, O.profile]
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


setCookieBs :: SetCookie -> ByteString
setCookieBs = BL.toStrict . toLazyByteString . renderSetCookie

expiredCookie :: ByteString -> SetCookie
expiredCookie n = defaultSetCookie
  { setCookieName    = n
  , setCookieValue   = ""
  , setCookieExpires = parseCookieExpires "Thu, 01 Jan 1970 00:00:00 GMT"
  , setCookiePath    = Just "/"
  }

expiredCookieHeader :: IsString a => ByteString -> (a, ByteString)
expiredCookieHeader n = ("Set-Cookie", setCookieBs (expiredCookie n))

makeSessionCookieBs :: CookieSettings -> ByteString -> SetCookie
makeSessionCookieBs cookieSettings jwt =
  applySessionCookieSettings cookieSettings
    $ applyCookieSettings cookieSettings
    $ def { setCookieValue = jwt }

handleLoginSuccess
  :: (MonadError ServerError m, MonadIO m)
  => OIDCEnv
  -> Text -- ^ code
  -> Text -- ^ state
  -> Maybe SessionCookie
  -> Maybe RedirectCookie
  -> m NoContent
handleLoginSuccess oidcenv oauthCode state (Just cookie) mredir = do
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
      let sessionCookie =
            makeSessionCookieBs (oidcCookieSettings oidcenv)
              $ Text.encodeUtf8
              $ O.accessToken tokens
      throwError err302
        { errHeaders = [ ("Location"  , maybe "/" getRedirectCookie mredir)
                       , ("Set-Cookie", setCookieBs sessionCookie)
                       , expiredCookieHeader "redirect"
                       , expiredCookieHeader "oidc-session"
                       ]
        }
    else forbidden "Please verify your email"
 where
  setType :: O.Tokens AuthInfo -> O.Tokens AuthInfo
  setType = id

handleLoginSuccess _ _ _ Nothing _ = forbidden "Session cookie missing"

handleLoginFailed
  :: (MonadError ServerError m)
  => Text -- ^ error
  -> m NoContent
handleLoginFailed = forbidden

appToErr :: ServerError -> Text -> ServerError
appToErr x msg = x
  { errBody    = Aeson.encode (Message msg (Text.pack $ errReasonPhrase x))
  , errHeaders = [ ("Content-Type", "text/json")
                 , expiredCookieHeader "redirect"
                 , expiredCookieHeader "oidc-session"
                 ]
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


-- * JWT

-- | Discover the JWK keys of the openid connect provider
discoverJWKs :: URI -> IO Jose.JWKSet
discoverJWKs uri = do
  manager <- newManager tlsManagerSettings

  cfgReq  <- requestFromURI uri
    { uriPath = uriPath uri <> ".well-known/openid-configuration"
    }
  cfgResp  <- httpLbs cfgReq manager
  jwksUri  <- either fail pure $ Aeson.eitherDecode (responseBody cfgResp)

  jwksReq  <- requestFromURI (jwks_uri jwksUri)
  jwksResp <- httpLbs jwksReq manager
  either fail pure $ Aeson.eitherDecode (responseBody jwksResp)

generateJwtSettings :: URI -> IO JWTSettings
generateJwtSettings uri = do
  myKey <- generateKey
  jwks  <- discoverJWKs uri
  pure $ (defaultJWTSettings myKey) { validationKeys = jwks }

newtype JWKSUri = JWKSUri { jwks_uri :: URI } deriving (Eq, Show, Generic)

instance FromJSON JWKSUri

