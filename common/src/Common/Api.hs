{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Common.Api
  ( Api'
  , BlogApi'
  , ClientApi
  , clientApi
  )
where


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
type Api' be = ("blogs" :> BlogApi' be)

-- BLOGS

-- | A slightly different api than the generic crud api. This is because we
-- need to handle the permissions differently.
-- It really is a different api therefore, but we can still reuse our existing generic implementation
-- brittany-disable-next-binding
type BlogApi' be
  = (Auth Admin :> Get_ BlogT)
      :<|>
      (Auth Admin :> Put_ BlogT)
      :<|>
      (Auth Admin :> Patch_ BlogT)
      :<|>
      (Auth Admin :> Delete_ BlogT)
      :<|>
      (Auth Admin :> DeleteList_ BlogT)
      :<|>
      (Auth Admin :> Post_ BlogT)
      :<|>
      (Auth Admin :> PostList BlogT)
      :<|>
      (GetList be BlogT)


type ClientApi = Api' (MockSqlBackend SqlSyntaxBuilder)

clientApi :: Proxy ClientApi
clientApi = Proxy
