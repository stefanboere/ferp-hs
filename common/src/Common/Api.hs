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
type WorldReadAdminWrite t be
  = Get_ t
      :<|>
      (Auth Admin :> Put_ t)
      :<|>
      (Auth Admin :> Patch_ t)
      :<|>
      (Auth Admin :> Delete_ t)
      :<|>
      (Auth Admin :> DeleteList_ be t)
      :<|>
      (Auth Admin :> Post_ t)
      :<|>
      (Auth Admin :> PostList t)
      :<|>
      (GetList be t)

type BlogApi' be = WorldReadAdminWrite BlogT be

type ChannelApi' be = WorldReadAdminWrite ChannelT be

type ClientApi = Api' Be

type Be = MockSqlBackend SqlSyntaxBuilder

clientApi :: Proxy ClientApi
clientApi = Proxy
