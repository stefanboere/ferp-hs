{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Api
Description: Specifies the combined api
-}
module Api
  ( Api
  , api
  , server
  )
where

import           Prelude                 hiding ( div )

import           Servant

import           Context

-- | The api
type Api = Get '[JSON] ()

-- | A proxy of the api
api :: Proxy Api
api = Proxy

-- | The combined server
server :: AppServer Api
server = pure ()

