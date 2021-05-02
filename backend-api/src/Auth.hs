{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Auth
  ( module Common.Auth
  )
where


import qualified Crypto.JWT                    as Jose
import           Data.Aeson
import qualified Data.Text                     as Text
import           GHC.Exts                       ( toList )
import           Lens.Micro                     ( (^.) )
import           Servant
import           Servant.AccessControl.Server
import qualified Servant.Auth.Server           as SAS

import           Common.Auth


instance (ForceAuthConstraints xs api ctxs auths v r)
  => HasServer (Auth' auths v r :> api) ctxs where
  type ServerT (Auth' auths v r :> api) m = v -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route = routeForceAuth


instance SAS.FromJWT AuthUser where
  decodeJWT m = case fromJSON (Object (m ^. Jose.unregisteredClaims)) of
    Error   e -> Left $ Text.pack e
    Success a -> Right a

instance SAS.ToJWT AuthUser where
  encodeJWT m = case toJSON m of
    Object x ->
      foldr (\(a, b) c -> Jose.addClaim a b c) Jose.emptyClaimsSet (toList x)
    _ -> Jose.emptyClaimsSet
