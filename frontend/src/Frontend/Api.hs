{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Api
  ( module Frontend.Api
  , AppT
  )
where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Functor.Identity          ( Identity )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Time                      ( NominalDiffTime )
import           Language.Javascript.JSaddle    ( MonadJSM )
import           Reflex.Dom              hiding ( Client
                                                , Link(..)
                                                , rangeInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.AccessControl          ( Token(..) )
import           Servant.Crud.API
import           Servant.Crud.Headers           ( ExceptLimited(..) )
import           Servant.Crud.QueryOperator     ( Filter )
import qualified Servant.Subscriber.Reflex     as Sub
import           Servant.Subscriber.Reflex      ( FreeClient )

import           Frontend.Context               ( AppT )
import           Common.Api
import           Common.Schema

-- | Supplying a token is not needed for xhr because the cookie is sent.
-- This is a utility for this.
usingCookie :: Token
usingCookie = Token ""

refreshAccessTokenEvery
  :: (Applicative m, Prerender js t m) => NominalDiffTime -> m ()
refreshAccessTokenEvery interval = prerender_ (pure ()) $ do
  tickEv <- tickLossyFromPostBuildTime interval
  refreshAccessTokenXhr (() <$ tickEv)

refreshAccessTokenXhr
  :: ( MonadIO m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , HasJSContext (Performable m)
     , TriggerEvent t m
     )
  => Event t ()
  -> m ()
refreshAccessTokenXhr ev = ignore <$> getAndDecode ("/auth/refresh" <$ ev)
 where
  ignore :: Event t (Maybe ()) -> ()
  ignore _ = ()

getBlog :: BlogNId -> FreeClient BlogN1
putBlog :: Token -> BlogId -> Blog -> FreeClient NoContent
patchBlog :: Token -> BlogId -> BlogPatch -> FreeClient NoContent
deleteBlog :: Token -> BlogId -> FreeClient NoContent
deleteBlogs
  :: Token -> ExceptLimited [BlogNId] -> BlogN Filter -> FreeClient [BlogNId]
postBlog :: Token -> Blog -> FreeClient (Headers '[LocationHdr] BlogId)
postBlogs :: Token -> [Blog] -> FreeClient [BlogId]
getBlogs :: View Be BlogN -> FreeClient (GetListHeaders BlogN1)
getBlogLabels
  :: View Be BlogN
  -> Maybe BlogNId
  -> FreeClient (GetListHeaders (Named BlogN Identity))


getChannel :: ChannelId -> FreeClient Channel
putChannel :: Token -> ChannelId -> Channel -> FreeClient NoContent
patchChannel :: Token -> ChannelId -> ChannelPatch -> FreeClient NoContent
deleteChannel :: Token -> ChannelId -> FreeClient NoContent
deleteChannels
  :: Token
  -> ExceptLimited [ChannelId]
  -> ChannelT Filter
  -> FreeClient [ChannelId]
postChannel :: Token -> Channel -> FreeClient (Headers '[LocationHdr] ChannelId)
postChannels :: Token -> [Channel] -> FreeClient [ChannelId]
getChannels :: View Be ChannelT -> FreeClient (GetListHeaders Channel)
getChannelLabels
  :: View Be ChannelT
  -> Maybe ChannelId
  -> FreeClient (GetListHeaders (Named ChannelT Identity))

-- brittany-disable-next-binding
(getBlog :<|> putBlog :<|> patchBlog :<|> deleteBlog :<|> deleteBlogs :<|> postBlog :<|> postBlogs :<|> getBlogs :<|> getBlogLabels) :<|>
  (getChannel :<|> putChannel :<|> patchChannel :<|> deleteChannel :<|> deleteChannels :<|> postChannel :<|> postChannels :<|> getChannels :<|> getChannelLabels)
  = Sub.client clientApi

getBlogsApiLink :: View Be BlogN -> Servant.API.Link
getBlogsApiLink =
  safeLink clientApi (Proxy :: Proxy ("blogs" :> GetList Be BlogN))

getChannelsApiLink :: View Be ChannelT -> Servant.API.Link
getChannelsApiLink =
  safeLink clientApi (Proxy :: Proxy ("channels" :> GetList Be ChannelT))
