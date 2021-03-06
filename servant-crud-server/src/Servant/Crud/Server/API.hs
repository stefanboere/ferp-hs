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
import qualified Data.Csv                      as Csv
import           Data.Swagger.ParamSchema       ( ToParamSchema(..) )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy.Encoding       as Text
import           Servant
import qualified Servant.CSV.Cassava           as CSV
import           Servant.Crud.API
import           Servant.Crud.Server.OrderBy    ( OrderBy
                                                , Selectors
                                                )
import           Servant.Crud.Server.QueryObject
                                                ( ToParams(..) )
import           Servant.Docs                   ( toParam )
import           Servant.Foreign                ( HasForeignType(..) )
import           Test.QuickCheck                ( Arbitrary(..) )


instance Accept CSV where
  contentType _ = contentType (Proxy :: Proxy CSV.CSV)
  contentTypes _ = contentTypes (Proxy :: Proxy CSV.CSV)

instance (Csv.DefaultOrdered a, Csv.ToNamedRecord a) => MimeRender CSV [a] where
  mimeRender _ = mimeRender (Proxy :: Proxy CSV.CSV)

instance (Csv.FromNamedRecord a) => MimeUnrender CSV [a] where
  mimeUnrender _ = fmap coerce . mimeUnrender (Proxy :: Proxy CSV.CSV)
  mimeUnrenderWithType _ x =
    fmap coerce . mimeUnrenderWithType (Proxy :: Proxy CSV.CSV) x

coerce :: (Csv.Header, [a]) -> [a]
coerce = snd

-- | Removes a Maybe at the cost of a 500 error
justOr500 :: MonadError ServerError m => Text -> Maybe a -> m a
justOr500 _   (Just x) = pure x
justOr500 err Nothing  = throwError $ err500 { errBody = Text.encodeUtf8 err }


-- | Removes a Maybe at the cost of a 404 error
justOr404 :: MonadError ServerError m => Text -> Maybe a -> m a
justOr404 _   (Just x) = pure x
justOr404 err Nothing  = throwError $ err404 { errBody = Text.encodeUtf8 err }

instance Arbitrary Page where
  arbitrary = Page <$> arbitrary <*> arbitrary

instance (Arbitrary Page, Selectors c r, Arbitrary filterType)
  => Arbitrary (View' c r filterType) where
  arbitrary = View <$> arbitrary <*> arbitrary <*> arbitrary

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
