{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Servant.Crud.Server.API
Description: Utilities for building API's
-}
module Servant.Crud.Server.API
  ( justOr404
  , justOr500
  , module Servant.Crud.API
  )
where

import           Prelude

import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Data.Swagger.ParamSchema       ( ToParamSchema(..) )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy.Encoding       as Text
import           Servant                        ( ServerError
                                                , errBody
                                                , err404
                                                , err500
                                                , Proxy(..)
                                                , QueryParams
                                                )
import           Servant.Docs                   ( toParam )
import           Servant.Crud.API
import           Servant.Crud.Server.OrderBy    ( OrderBy
                                                , Selectors
                                                )
import           Servant.Crud.Server.QueryObject
                                                ( ToParams(..) )
import           Servant.Foreign                ( HasForeignType(..) )

-- | Removes a Maybe at the cost of a 500 error
justOr500 :: MonadError ServerError m => Text -> Maybe a -> m a
justOr500 _   (Just x) = pure x
justOr500 err Nothing  = throwError $ err500 { errBody = Text.encodeUtf8 err }


-- | Removes a Maybe at the cost of a 404 error
justOr404 :: MonadError ServerError m => Text -> Maybe a -> m a
justOr404 _   (Just x) = pure x
justOr404 err Nothing  = throwError $ err404 { errBody = Text.encodeUtf8 err }


instance
    ( Selectors c r
    , HasForeignType lang ftype (OrderBy c r)
    , ToParams lang ftype Page
    , ToParams lang ftype filterType
    )
  => ToParams lang ftype (View' c r filterType) where
  toParams t lang ftype _ = concat
    [ toParams t lang ftype (Proxy :: Proxy Page)
    , toParams t lang ftype (Proxy :: Proxy filterType)
    , [ ( toParam (Proxy :: Proxy (QueryParams "" (OrderBy c r)))
        , toParamSchema (Proxy :: Proxy (OrderBy c r))
        , typeFor lang ftype (Proxy :: Proxy (OrderBy c r))
        )
      ]
    ]
