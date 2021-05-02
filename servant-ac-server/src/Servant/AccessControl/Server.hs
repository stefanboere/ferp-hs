{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Servant.AccessControl.Server
Description: Orphan instances for `Servant.AccessControl`
Stability: experimental

This module provides orphan instances for @Servant.AccessControl@ and
utilities for writing @HasServer@ instances.

-}
module Servant.AccessControl.Server
  ( -- * The combinators
    Auth'
  , HasAccessControl(..)
  , Everyone
  , Nobody
  , AuthData(..)
  -- * Advanced stuff
  -- | You will typically only need to look at this if you are adding type level stuff
  -- to Servant yourself, e.g. you are implementing a new endpoint type.
  , module Servant.AddSetHeader
  -- * HasServer instance helpers
  , ForceAuthConstraints
  , routeForceAuth
  , authCheck
  )
where

import           Prelude

import           Control.Monad.IO.Class         ( liftIO )
import           Data.String                    ( fromString )
import           Servant
import           Servant.AccessControl
import           Servant.AddSetHeader
import           Servant.Aeson.Internal         ( HasGenericSpecs
                                                , collectRoundtripSpecs
                                                )
import           Servant.Auth.Server            ( AreAuths
                                                , Auth
                                                , AuthResult(..)
                                                , CookieSettings
                                                , JWTSettings
                                                , SetCookie
                                                , ThrowAll
                                                , ToJWT
                                                , makeSessionCookie
                                                , makeXsrfCookie
                                                , runAuthCheck
                                                , throwAll
                                                )
import           Servant.Auth.Server.Internal.Class
                                                ( runAuths )
import           Servant.Docs                   ( HasDocs
                                                , docsFor
                                                )
import           Servant.Ekg                    ( HasEndpoint
                                                , enumerateEndpoints
                                                , getEndpoint
                                                )
import           Servant.Foreign                ( Arg(..)
                                                , Foreign
                                                , HasForeign
                                                , HasForeignType(..)
                                                , HeaderArg(..)
                                                , PathSegment(..)
                                                , Req(..)
                                                , foreignFor
                                                )
import           Servant.QuickCheck.Internal.HasGenRequest
                                                ( HasGenRequest
                                                , genRequest
                                                )
import           Servant.Server.Internal.Delayed
                                                ( Delayed
                                                , addAuthCheck
                                                )
import           Servant.Server.Internal.DelayedIO
                                                ( DelayedIO
                                                , withRequest
                                                )
import           Servant.Server.Internal.Router ( Router )
import           Servant.Swagger                ( HasSwagger
                                                , toSwagger
                                                )



type HSetCookie = '("Set-Cookie", SetCookie)

-- | Constraints needed for @routeForceAuth@
type ForceAuthConstraints xs api ctxs auths v r
  = ( xs ~ '[ HSetCookie, HSetCookie]
    , HasServer (AddSetHeadersApi xs api) ctxs
    , AreAuths auths ctxs v
    , HasServer api ctxs -- this constraint is needed to implement hoistServer
    , AddSetHeaders
        xs
        (ServerT api Handler)
        (ServerT (AddSetHeadersApi xs api) Handler)
    , ToJWT v
    , ToWwwAuthenticate (Auth' auths v r)
    , HasContextEntry ctxs CookieSettings
    , HasContextEntry ctxs JWTSettings
    , HasAccessControl v r
    , ThrowAll (ServerT api Handler)
    )

-- | Returns 401 if the authentiation failed, and 403 if the autorization failed.
-- Also adds a WWW-Authenticate header.
--
-- Can be used to implement a @HasServer@ instance for which your handler is only
-- called if the authentiation is succesful.
-- This instance returns 401 or 403 on authentiation or authorization failure.
--
--       instance (ForceAuthConstraints xs api ctxs auths v r)
--         => HasServer (Auth' auths v r :> api) ctxs where
--         type ServerT (Auth' auths v r :> api) m = v -> ServerT api m
--
--         hoistServerWithContext _ pc nt s =
--           hoistServerWithContext (Proxy :: Proxy api) pc nt . s
--
--         route = routeForceAuth
--
routeForceAuth
  :: forall xs api ctxs auths v r env
   . ( ForceAuthConstraints xs api ctxs auths v r
     , Server (Auth' auths v r :> api) ~ (v -> ServerT api Handler)
     )
  => Proxy (Auth' auths v r :> api)
  -> Context ctxs
  -> Delayed env (Server (Auth' auths v r :> api))
  -> Router env
routeForceAuth _ context subserver = route
  (Proxy :: Proxy (AddSetHeadersApi xs api))
  context
  (              fmap (go . forceAuth) subserver
  `addAuthCheck` authCheck (Proxy :: Proxy auths) context
  )

 where
  forceAuth :: (v -> Server api) -> AuthResult v -> Server api
  forceAuth server (Authenticated v) =
    if hasAccess (Proxy :: Proxy r) v then server v else throwAll err403
  forceAuth _ _ = throwAll
    (err401
      { errHeaders = maybe []
                           (\auth -> [("WWW-Authenticate", fromString auth)])
                           hAuth
      }
    )

  hAuth = toWwwAuthenticate (Proxy :: Proxy (Auth' auths v r))

  go
    :: (new ~ ServerT (AddSetHeadersApi xs api) Handler)
    => (AuthResult v -> ServerT api Handler)
    -> (AuthResult v, SetHeaderList xs)
    -> new
  go fn (authResult, cookies) = addSetHeaders cookies $ fn authResult

authCheck
  :: forall auths cntx v
   . ( AreAuths auths cntx v
     , ToJWT v
     , HasContextEntry cntx JWTSettings
     , HasContextEntry cntx CookieSettings
     )
  => Proxy (auths :: [*])
  -> Context cntx
  -> DelayedIO (AuthResult v, SetHeaderList '[HSetCookie, HSetCookie])
authCheck pauth context = withRequest $ \req -> liftIO $ do
  authResult <- runAuthCheck (runAuths pauth context) req
  cookies    <- makeCookies authResult
  return (authResult, cookies)
 where

  jwtSettings :: JWTSettings
  jwtSettings = getContextEntry context

  cookieSettings :: CookieSettings
  cookieSettings = getContextEntry context

  pCookie :: Proxy "Set-Cookie"
  pCookie = Proxy


  makeCookies :: AuthResult v -> IO (SetHeaderList '[HSetCookie, HSetCookie])
  makeCookies authResult = do
    xsrf <- makeXsrfCookie cookieSettings
    fmap (SetHeaderCons pCookie (Just xsrf)) $ case authResult of
      (Authenticated v) -> do
        ejwt <- makeSessionCookie cookieSettings jwtSettings v
        case ejwt of
          Nothing  -> return $ SetHeaderCons pCookie Nothing SetHeaderNil
          Just jwt -> return $ SetHeaderCons pCookie (Just jwt) SetHeaderNil
      _ -> return $ SetHeaderCons pCookie Nothing SetHeaderNil


instance HasEndpoint (sub :: *) => HasEndpoint (Auth' t a r :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasGenericSpecs api  => HasGenericSpecs (Auth' y x r :> api) where
  collectRoundtripSpecs settings Proxy =
    collectRoundtripSpecs settings (Proxy :: Proxy api)

instance (HasGenRequest a) => HasGenRequest (Auth' x y r :> a) where
  genRequest _ = genRequest (Proxy :: Proxy a)

instance HasDocs (Auth auths v :> api)
  => HasDocs (Auth' auths v r :> api) where
  docsFor _ = docsFor (Proxy :: Proxy (Auth auths v :> api))

instance HasSwagger (Auth auths v :> api)
  => HasSwagger (Auth' auths v r :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Auth auths v :> api))

instance HasForeign lang ftype (Auth auths v :> api)
  => HasForeign lang ftype (Auth' auths v r :> api) where
  type Foreign ftype (Auth' auths v r :> api)
    = Foreign ftype (Auth auths v :> api)
  foreignFor lang ftype _ =
    foreignFor lang ftype (Proxy :: Proxy (Auth auths v :> api))


instance HasEndpoint (api :: *) => HasEndpoint (Auth x y :> api) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy api)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy api)

instance HasGenericSpecs api  => HasGenericSpecs (Auth x y :> api) where
  collectRoundtripSpecs settings Proxy =
    collectRoundtripSpecs settings (Proxy :: Proxy api)

instance (HasGenRequest api) => HasGenRequest (Auth x y :> api) where
  genRequest _ = genRequest (Proxy :: Proxy api)

-- Nothing to do for no auth options
instance {-# OVERLAPPABLE #-} HasForeign lang ftype api
    => HasForeign lang ftype (Auth '[] y :> api) where
  type Foreign ftype (Auth '[] y :> api) = Foreign ftype api

  foreignFor lang ftype Proxy = foreignFor lang ftype (Proxy :: Proxy api)

instance {-# OVERLAPPING #-}
  ( HasForeign lang ftype api
  , HasForeignType lang ftype (AuthData xs)
  )
    => HasForeign lang ftype (Auth xs y :> api) where
  type Foreign ftype (Auth xs y :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api)
      $ req { _reqHeaders = _reqHeaders req ++ authHeader }
   where
    authHeader =
      [ HeaderArg
          (Arg (PathSegment "Authorization")
               (typeFor lang ftype (Proxy :: Proxy (AuthData xs)))
          )
      ]
