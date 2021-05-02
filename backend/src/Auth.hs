{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Auth
  ( module Common.Auth
  )
where


import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Crypto.JWT                    as Jose
import           Data.Aeson
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as BL
                                                ( fromStrict )
import qualified Data.Text                     as Text
import           GHC.Exts                       ( toList )
import           Lens.Micro                     ( (^.) )
import           Lucid
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Network.Wai                    ( rawPathInfo
                                                , rawQueryString
                                                )
import           Servant
import           Servant.AccessControl.Server
import           Servant.Auth.Server            ( AuthResult(..)
                                                , FromJWT(..)
                                                , ToJWT(..)
                                                , throwAll
                                                )
import           Servant.Router                 ( View )
import           Servant.Server.Internal.Delayed
                                                ( Delayed
                                                , addAuthCheck
                                                )
import           Servant.Server.Internal.DelayedIO
                                                ( DelayedIO
                                                , withRequest
                                                )
import           Servant.Server.Internal.Router ( Router )
import           Web.Cookie                     ( SetCookie(..)
                                                , defaultSetCookie
                                                )

import           Common.Auth
import           OIDC

instance (ForceAuthConstraints xs api ctxs auths v r, HasContextEntry ctxs OIDCEnv)
  => HasServer (Auth' auths v r :> api) ctxs where
  type ServerT (Auth' auths v r :> api) m = ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt s

  route = routeRedirect

instance FromJWT AuthUser where
  decodeJWT m = case fromJSON (Object (m ^. Jose.unregisteredClaims)) of
    Error   e -> Left $ Text.pack e
    Success a -> Right a

instance ToJWT AuthUser where
  encodeJWT m = case toJSON m of
    Object x ->
      foldr (\(a, b) c -> Jose.addClaim a b c) Jose.emptyClaimsSet (toList x)
    _ -> Jose.emptyClaimsSet

data OidcResult = OidcResult
  { oidcResult_session  :: SetCookie
  , oidcResult_location :: ByteString
  , oidcResult_redirect :: SetCookie
  }

-- | Returns 302 if the authentiation failed, and 403 if the autorization failed.
-- Also adds a WWW-Authenticate header.
routeRedirect
  :: forall xs api ctxs auths v r env
   . ( ForceAuthConstraints xs api ctxs auths v r
     , HasContextEntry ctxs OIDCEnv
     )
  => Proxy (Auth' auths v r :> api)
  -> Context ctxs
  -> Delayed env (Server (Auth' auths v r :> api))
  -> Router env
routeRedirect _ context subserver = route
  (Proxy :: Proxy (AddSetHeadersApi xs api))
  context
  (fmap (go . forceAuth) subserver `addAuthCheck` combinedCheck)

 where
  forceAuth :: Server api -> Either OidcResult v -> Server api
  forceAuth server (Right v) =
    if hasAccess (Proxy :: Proxy r) v then server else throwAll err403
  forceAuth _ (Left oidcR) = throwAll err302
    { errHeaders = [ ("Location"  , oidcResult_location oidcR)
                   , ("Set-Cookie", setCookieBs (oidcResult_session oidcR))
                   , ("Set-Cookie", setCookieBs (oidcResult_redirect oidcR))
                   ]
    }

  combinedCheck :: DelayedIO (Either OidcResult v, SetHeaderList xs)
  combinedCheck = do
    (a, h) <- authCheck (Proxy :: Proxy auths) context
    r      <- case a of
      Authenticated v -> pure $ Right v
      _               -> Left <$> oidcCheck
    pure (r, h)


  oidcCheck :: DelayedIO OidcResult
  oidcCheck = withRequest $ \req -> liftIO $ do
    let oidcEnv = getContextEntry context
    (loc, sid) <- liftIO (genOIDCURL oidcEnv)
    let sessionCookie = defaultSetCookie { setCookieName  = "oidc-session"
                                         , setCookieValue = sid
                                         , setCookiePath  = Just "/auth/return"
                                         }
    let curLoc = rawPathInfo req <> rawQueryString req
    let redirectCookie = defaultSetCookie { setCookieName  = "redirect"
                                          , setCookieValue = curLoc
                                          , setCookiePath  = Just "/auth/return"
                                          }
    pure $ OidcResult { oidcResult_session  = sessionCookie
                      , oidcResult_location = loc
                      , oidcResult_redirect = redirectCookie
                      }

  go
    :: (new ~ ServerT (AddSetHeadersApi xs api) Handler)
    => (Either OidcResult v -> ServerT api Handler)
    -> (Either OidcResult v, SetHeaderList xs)
    -> new
  go fn (authResult, SetHeaderCons p x xs) =
    addSetHeaders (SetHeaderCons p (clear x) xs) $ fn authResult

  clear :: Maybe a -> Maybe a
  clear _ = Nothing

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ = BL.fromStrict

instance MimeRender HTML (Html ()) where
  mimeRender _ = renderBS

instance HasServer View context where
  type ServerT View m = m (Html ())
  hoistServerWithContext _ _ nt s = nt s

  route Proxy cnt action =
    Servant.route (Proxy :: Proxy (Get '[HTML] (Html ()))) cnt action

type instance AddSetHeaderApi sym t View
  = Verb 'GET 200 '[HTML] (AddSetHeaderApiVerb sym t (Html ()))

