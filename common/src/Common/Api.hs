{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Common.Api
  ( Api'
  , BlogApi'
  , ChannelApi'
  , ClientApi
  , clientApi
  , Be
  , View
  , ViewOrderBy
  , OrderByScope
  , GetList
  ) where


import           Data.Proxy
import           Database.Beam.API
import           Database.Beam.Backend.SQL      ( MockSqlBackend )
import           Database.Beam.Backend.SQL.Builder
                                                ( SqlSyntaxBuilder )
import           Servant.API

import           Common.Auth
import           Common.Schema


{-# ANN module ("HLint: ignore Redundant bracket" :: String) #-}
-- | The api
-- brittany-disable-next-binding
type Api' be = ("blogs" :> BlogApi' be)
   :<|> ("channels" :> ChannelApi' be)

-- BLOGS

-- | A slightly different api than the generic crud api. This is because we
-- need to handle the permissions differently.
-- It really is a different api therefore, but we can still reuse our existing generic implementation
-- brittany-disable-next-binding
type WorldReadAdminWrite t t0 be
  = Get_ t
      :<|>
      (Auth Admin :> Put_ t0)
      :<|>
      (Auth Admin :> Patch_ t0)
      :<|>
      (Auth Admin :> Delete_ t0)
      :<|>
      (Auth Admin :> DeleteList_ be t)
      :<|>
      (Auth Admin :> Post_ t0)
      :<|>
      (Auth Admin :> PostList t0)
      :<|>
      (GetList be t)
      :<|>
      (GetListLabels be t)

type BlogApi' be = WorldReadAdminWrite BlogN BlogT be

type ChannelApi' be = WorldReadAdminWrite ChannelT ChannelT be

type ClientApi = Api' Be

type Be = MockSqlBackend SqlSyntaxBuilder

clientApi :: Proxy ClientApi
clientApi = Proxy
