{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Protected
  ( protectedHandler
  , protectedLinks
  , ProtectedApi
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Proxy
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Crud.API               ( View'(..)
                                                , Page(..)
                                                )
import           Servant.Common.Req             ( reqSuccess )
import           Servant.Links           hiding ( URI(..) )
import           URI.ByteString                 ( URI )

import           Servant.Router

import           Common.Auth
import           Components
import           Frontend.Api


type ProtectedApi = Auth Everyone :> "user" :> "self" :> View

protectedApi :: Proxy ProtectedApi
protectedApi = Proxy

protectedSelfLink :: Link
protectedSelfLink = allLinks protectedApi

protectedLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
protectedLinks dynUri = safelinkGroup
  (text "Protected")
  [safelink dynUri protectedSelfLink $ text "User"]

protectedHandler
  :: WidgetConstraint js t m => RouteT ProtectedApi m (Event t URI)
protectedHandler = protectedSelf

protectedSelf
  :: forall js t m
   . (DomBuilder t m, PostBuild t m, MonadHold t m, Prerender js t m)
  => m (Event t URI)
protectedSelf = do
  el "h1" $ text "Current User"

  getBtn <- btn def (text "Fetch blogs")

  rEv <- prerender (pure never) $ fmapMaybe reqSuccess <$> getBlogs' vw getBtn
  r <- holdDyn Nothing (Just . getResponse <$> switchDyn rEv)
  display r

  pure never
  where vw = mempty { page = Page Nothing (Just 10) }



