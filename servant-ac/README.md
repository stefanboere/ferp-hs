# Servant Access Control

This library allows you to specify who has access to an endpoint in exactly
the same place where you specify the endpoint itself.

## Quickstart

This library only works if you use `servant-auth` for authentication,
so make sure you have that working.
Say you want to use 'Basic Auth', 'JWT' and 'Cookie' auth and the result of your
authentication process is `AuthUser`, then for convenience create the following type.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
...
import qualified Servant.Auth as SA
import           Servant.AccessControl.Server (Auth')

type Auth = Auth' '[SA.BasicAuth, SA.Cookie, SA.JWT] AuthUser
```

Then you can start by restricting api access as follows
(assuming your api is of type ` Api`):

```haskell
-- These endpoints are publically available
type PublicApi = Api

-- These endpoints are only available for authenticated users
type ProtectedApi = Auth Everyone :> Api

-- This endpoint is for no-one available. Useful for temporarily restricting access
type HiddenApi = Auth Nobody :> Api
```

## Role based access control

You might want to make certain parts of your app only available
for users with certain roles, e.g. you want to allow admin only
to the admin area.

The roles are regular values, but we want to put them in types,
so we will need to jump through some type level hoops. But fear not.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import         Servant.AccessControl.Server (HasAccessControl)
...

-- Create a type to express our logic.
data InRoles (roles :: [Role])

-- Put this in the type and only admins will be able to access the endpoints
--
-- > type AdminEndpoints = Auth Admin :> Api
type Admin = InRoles '[ 'Administrator]

-- Now we need to tell 'HasAccessControl' who actually has access
-- to the api.
-- If no roles are given, we will just reject everyone.
-- ( Nobody has any of the roles in the list).
instance HasAccessControl AuthUser (InRoles '[]) where
  -- hasAccess :: Proxy (InRoles '[]) -> AuthUser -> Bool
  hasAccess _ _ = False

-- Here 'KnownRole' is used to "unlift" the type level value to the regular context.
instance (KnownRole x, HasAccessControl AuthUser (InRoles xs))
    => HasAccessControl AuthUser (InRoles (x ': xs)) where
  hasAccess _ u =
    -- Some role in the list should be a role of the user (getUserRoles u)
    roleVal (Proxy :: Proxy x)
      `elem` getUserRoles u
      ||     hasAccess (Proxy :: Proxy (InRoles xs)) u

-- | These are the roles in our app
data Role
  = Administrator
  | ...

class KnownRole role where
  roleVal :: Proxy role -> Role

instance KnownRole 'Administrator where
  roleVal _ = Administrator
```

And that's it, that is the entire library.

## Remarks

This library is split up into 'servant-ac'  and
'servant-ac-server' libraries. The latter contains some orphan instances.
