{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Api
  ( module Frontend.Api
  , ApiWidget
  ) where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
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
import           Servant.Subscriber.Reflex      ( ApiWidget
                                                , FreeClient
                                                )

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

runApi
  :: (MonadHold t m, MonadFix m, Prerender js t m) => ApiWidget t m a -> m a
runApi = Sub.runApiWidget "ws://localhost:3005/subscriber"

getBlog :: BlogId -> FreeClient Blog
putBlog :: Token -> BlogId -> Blog -> FreeClient NoContent
patchBlog :: Token -> BlogId -> BlogPatch -> FreeClient NoContent
deleteBlog :: Token -> BlogId -> FreeClient NoContent
deleteBlogs
  :: Token -> ExceptLimited [BlogId] -> BlogT Filter -> FreeClient [BlogId]
postBlog :: Token -> Blog -> FreeClient (Headers '[LocationHdr] BlogId)
postBlogs :: Token -> [Blog] -> FreeClient [BlogId]
getBlogs :: View Be BlogT -> FreeClient (GetListHeaders Blog)


getChannel :: ChannelId -> FreeClient Channel
putChannel :: Token -> ChannelId -> Channel -> FreeClient NoContent
patchChannel :: Token -> ChannelId -> ChannelPatch -> FreeClient NoContent
deleteChannel :: Token -> ChannelId -> FreeClient NoContent
deleteChannels
  :: Token
  -> ExceptLimited [ChannelId]
  -> ChannelT Filter
  -> FreeClient [ChannelId]
postChannel
  :: Token -> Channel -> FreeClient (Headers '[LocationHdr] ChannelId)
postChannels :: Token -> [Channel] -> FreeClient [ChannelId]
getChannels :: View Be ChannelT -> FreeClient (GetListHeaders Channel)

-- brittany-disable-next-binding
(getBlog :<|> putBlog :<|> patchBlog :<|> deleteBlog :<|> deleteBlogs :<|> postBlog :<|> postBlogs :<|> getBlogs) :<|>
  (getChannel :<|> putChannel :<|> patchChannel :<|> deleteChannel :<|> deleteChannels :<|> postChannel :<|> postChannels :<|> getChannels)
  = Sub.client clientApi

getBlogsApiLink :: View Be BlogT -> Servant.API.Link
getBlogsApiLink =
  safeLink clientApi (Proxy :: Proxy ("blogs" :> GetList Be BlogT))

getChannelsApiLink :: View Be ChannelT -> Servant.API.Link
getChannelsApiLink =
  safeLink clientApi (Proxy :: Proxy ("channels" :> GetList Be ChannelT))
