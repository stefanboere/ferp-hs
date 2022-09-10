{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Common.Api
  ( Api
  , BlogApi
  , ChannelApi
  , api
  , View
  , GetList
  , OrderBy'
  , AttrName
  ) where


import           Data.Proxy
import           ProjectM36.Beamable
import           Servant.API

import           Common.Auth
import           Common.Schema


{-# ANN module ("HLint: ignore Redundant bracket" :: String) #-}
-- | The api
-- brittany-disable-next-binding
type Api = ("blogs" :> BlogApi)
   :<|> ("channels" :> ChannelApi)

-- BLOGS

-- | A slightly different api than the generic crud api. This is because we
-- need to handle the permissions differently.
-- It really is a different api therefore, but we can still reuse our existing generic implementation
-- brittany-disable-next-binding
type WorldReadAdminWrite t t0
  = Get_ t
      :<|>
      (Auth Admin :> Put_ t0)
      :<|>
      (Auth Admin :> Patch_ t0)
      :<|>
      (Auth Admin :> Delete_ t0)
      :<|>
      (Auth Admin :> DeleteList_ t)
      :<|>
      (Auth Admin :> Post_ t0)
      :<|>
      (Auth Admin :> PostList t0)
      :<|>
      (GetList t)
      :<|>
      (GetListLabels t)

-- brittany-disable-next-binding
type WorldReadWrite t t0
  = Get_ t
      :<|>
      (Put_ t0)
      :<|>
      (Patch_ t0)
      :<|>
      (Delete_ t0)
      :<|>
      (DeleteList_ t)
      :<|>
      (Post_ t0)
      :<|>
      (PostList t0)
      :<|>
      (GetList t)
      :<|>
      (GetListLabels t)

type BlogApi = WorldReadAdminWrite BlogN BlogT

type ChannelApi = WorldReadWrite ChannelT ChannelT

api :: Proxy Api
api = Proxy
