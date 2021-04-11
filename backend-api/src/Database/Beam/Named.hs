{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Database.Beam.Named
Description: Upgrade your records to return names together with keys in GET requests
-}
module Database.Beam.Named
  ( module Database.Beam.Expand
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Swagger                   ( NamedSchema(..)
                                                , SwaggerType(..)
                                                , ToSchema(..)
                                                , properties
                                                , required
                                                , toSchema
                                                , type_
                                                )
import qualified Data.Swagger                  as Swagger
                                                ( Referenced(Inline) )
import           Data.Text                      ( Text )
import           Database.Beam           hiding ( set )
import           Database.Beam.Expand
import           Lens.Micro
import           Servant.Crud.Server.QueryObject
                                                ( ToParams
                                                , toParams
                                                )

instance ToSchema (PrimaryKey t Identity) => ToSchema (Named t Identity) where
  declareNamedSchema _ = pure $ NamedSchema (Just "Named") schema
   where
    schema =
      mempty
        & set type_    (Just SwaggerObject)
        & set required ["id", "name"]
        & set
            properties
            [ ("id"  , Swagger.Inline (toSchema idProxy))
            , ("name", Swagger.Inline (toSchema nameProxy))
            ]

    idProxy :: Proxy (PrimaryKey t Identity)
    idProxy = Proxy

    nameProxy :: Proxy Text
    nameProxy = Proxy

instance (ToParams lang ftype (PrimaryKey t f), ToParams lang ftype (C f Text))
    => ToParams lang ftype (Named t f) where
  toParams p lang ftype _ =
    toParams (p <> ".id") lang ftype idProxy ++ toParams p lang ftype nameProxy
   where
    idProxy :: Proxy (PrimaryKey t f)
    idProxy = Proxy

    nameProxy :: Proxy (C f Text)
    nameProxy = Proxy