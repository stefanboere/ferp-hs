{-# LANGUAGE DataKinds #-}
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
{-|
Module: Servant.AccessControl
Description: Types for inline, type level, access control rules
Stability: experimental

Permissions are hard to do right. There are many operations on data,
like updating, inserting, deleting data, calling procedures, querying data.
Each role should have the right settings to make this work. It is very easy to forget
to grant or deny permissions, especially if the permissions are managed
in a different place. Furthermore, it is usually not clear at all
who has access to certain data.

We should keep the AC rules very close to the route definitions.
It should be a /type/.
In this module we present the 'Auth'' type, which does two things:

- It authenticates users in exactly the same way as 'Servant.Auth.Server.Auth' does.
- It grants or denies users access based on extra meta information present in the type

This approach has a couple of advantages

- The AC rules are very close to the actual implementation
- It is easy to see who has access to certain routes, making it easier
   to spot security holes.
- Managing rules is easy. If the route joins other data, there is no need
   to manage ac rules for this data. If the user has access to a route, it works.

= Usage
First create a type which specifies your authentication scheme.
The first two arguments to @Auth'@ are the same as for @Auth@, it takes a list of
authentication mechanisms and the return type of a successful authentication.

@
type Auth = Auth' '[SA.BasicAuth, SA.Cookie, SA.JWT] AuthUser
@

Then you can start by restricting api access as follows (assuming your api is of type @Api@)

@
-- These endpoints are publically available
type PublicApi = Api

-- These endpoints are only available for authenticated users
type ProtectedApi = Auth Everyone :> Api

-- This endpoint is for no-one available. Useful for temporarily restricting access
type HiddenApi = Auth Nobody :> Api
@

We understand that each application is it's own little snow flake, so just 'Everyone'
and 'Nobody' will not do. See 'HasAccessControl' how to implement an actually useful
authentication rule.

-}
module Servant.AccessControl
  ( -- * The combinators
    Auth'
  , HasAccessControl(..)
  , Everyone
  , Nobody
  , AuthData(..)
  , ToWwwAuthenticate(..)
  -- * Re-exports
  , Token(..)
  )
where

import           Prelude

import           Data.Kind                      ( Constraint )
import           Data.Proxy
import           Servant.API
import           Servant.Auth.Client            ( Token(..) )
import           Servant.Auth                   ( Auth
                                                , JWT
                                                , Cookie
                                                , BasicAuth
                                                )
import           Servant.Client.Core            ( HasClient
                                                , Client
                                                , RunClient
                                                , clientWithRoute
                                                , hoistClientMonad
                                                )

-- | Like 'Servant.Auth.Server.Auth', buth forces the auth check to be valid
--
-- Instead of @AuthResult v@ you will just have @v@
-- This means that you only have to handle requests coming from valid users.
-- It furthermore has an extra argument which allows you to tweak access control
-- to users based on roles, etc.
--
-- For it's usage see 'HasAccessControl'
-- The type @Auth' auths v Everyone@ is approximately 2Auth auths v@
data Auth' (auths :: [*]) v r

class ToWwwAuthenticate auth where
  toWwwAuthenticate :: Proxy auth -> Maybe String

instance ToWwwAuthenticate JWT where
  toWwwAuthenticate _ = Just "Bearer"

instance ToWwwAuthenticate Servant.Auth.BasicAuth where
  toWwwAuthenticate _ = Just "Basic"

instance ToWwwAuthenticate Cookie where
  toWwwAuthenticate _ = Nothing

instance ToWwwAuthenticate x
  => ToWwwAuthenticate (Auth' (x ': '[]) v r) where
  toWwwAuthenticate _ = toWwwAuthenticate (Proxy :: Proxy x)

instance (ToWwwAuthenticate x, ToWwwAuthenticate (Auth' (y ': xs) v r))
  => ToWwwAuthenticate (Auth' ( x ': y ': xs ) v r) where
  toWwwAuthenticate _ = case mx of
    Just x -> case mxs of
      Just xs -> Just $ x <> ", " <> xs
      Nothing -> Just x
    Nothing -> mxs
   where
    mx  = toWwwAuthenticate (Proxy :: Proxy x)
    mxs = toWwwAuthenticate (Proxy :: Proxy (Auth' (y ': xs) v r))

instance HasLink sub => HasLink (Auth' auths v r :> sub)  where
  type MkLink (Auth' auths v r :> sub) a = MkLink sub a
  toLink toA _ = toLink toA (Proxy :: Proxy sub)

type family HasJWT xs :: Constraint where
  HasJWT (JWT ': xs) = ()
  HasJWT (x ': xs)   = HasJWT xs
  HasJWT '[]         = JWTAuthNotEnabled

class JWTAuthNotEnabled

-- | @'HasJWT' auths@ is nominally a redundant constraint, but ensures we're not
-- trying to send a token to an API that doesn't accept them.
instance (HasJWT auths, RunClient m, HasClient m (Auth auths a :> api))
    => HasClient m (Auth' auths a r :> api) where
  type Client m (Auth' auths a r :> api) = Token -> Client m api

  clientWithRoute m _ =
    clientWithRoute m (Proxy :: Proxy (Auth auths a :> api))

  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy :: Proxy (Auth auths a :> api))


-- | The "a" type allows you to use 'Auth' to enrich it for Role based Access Control.
-- Suppose you want to guard your app using the role names, define
--
-- > data InRoles (roles :: [Symbol])
--
-- Now implement 'HasAccessControl AuthUser (InRoles roles)'
--
-- @
--  -- If a user has no roles, deny access
-- instance HasAccessControl AuthUser (InRoles '[]) where
--    hasAccess _ _ = False
--
-- instance (KnownSymbol x, HasAccessControl AuthUser (InRoles xs))
--     => HasAccessControl AuthUser (InRoles (x ': xs)) where
--  hasAccess _ u =
--    -- Either the user has the first valid role
--    symbolVal (Proxy :: Proxy x) \`elem\` getUserRoles u
--    -- Or one of the other roles
--       ||     hasAccess (Proxy :: Proxy (InRoles xs)) u
-- @
--
-- Then you can guard admin routes as follows
--
-- > type IsAdmin = InRoles ("admin" ': '[])
-- > type AdminApi = Auth IsAdmin :> Api
--
-- === Sum types for roles instead of strings.
-- Using strings to distinguish roles might not be a good idea. Here is an example
-- how to replace @Symbol@  by @Role@.
--
-- > data Role
-- >   = Administrator
-- >   | ...
-- >
-- > class KnownRole role where
-- >   roleVal :: Proxy role -> Role
-- >
-- > instance KnownRole 'Administrator where
-- >   roleVal _ = Administrator
-- > ...
--
--
class HasAccessControl v a where
  hasAccess :: Proxy a -> v -> Bool

-- | No additional checks, i.e. has trivial instance for 'HasAccessControl'
data Everyone

instance HasAccessControl v Everyone where
  hasAccess _ _ = True

-- | Deny all users (logged in and not logged in).
-- Useful for temporarily disabeling routes (if it is not temporarily, why bother keeping this route).
data Nobody

instance HasAccessControl v Nobody where
  hasAccess _ _ = False


-- | Heterogene list of possible auth data
-- Currently only JWT and BasicAuth are included. This is due to the opinion of the
-- library author that these are better than other auth data.
data AuthData xs where
  NoAuth ::AuthData '[]
  JWTAuth ::Token -> AuthData xs -> AuthData (JWT ': xs)
  BasicAuth ::BasicAuthData -> AuthData xs -> AuthData (Servant.Auth.BasicAuth ': xs)
