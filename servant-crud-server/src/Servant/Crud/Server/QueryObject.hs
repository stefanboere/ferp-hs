{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: Servant.Crud.Server.QueryObject
Description: Servant server implementation of QueryObject

This module contains instances for 'HasServer', the generation of Docs and
generic querying of the Api.

The main documentation is at "Servant.Crud.QueryObject".

To make it work with auto generated docs (servant-docs and servant-swagger), see 'ToParams'.
-}
module Servant.Crud.Server.QueryObject
  (
-- * ToParams
    ToParams
  , toParams
  , toParams1
  , defaultToParams
  , addDocPrefix
  , toQueryArg
  -- * Generics
  , GToParams
  -- * Re-Exports
  , module Servant.Crud.QueryObject
  )
where

import           Prelude
import           Servant.Crud.QueryObject


import           Data.List
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as Text
                                                ( fromStrict )
import qualified Data.Text.Lazy.Encoding       as Text
                                                ( encodeUtf8 )

import           Data.Swagger.Internal          ( Param(Param)
                                                , ParamAnySchema(..)
                                                , ParamLocation(..)
                                                , ParamOtherSchema(..)
                                                , ParamSchema
                                                )
import           Data.Swagger.ParamSchema       ( ToParamSchema(..) )
import           GHC.Generics
import           GHC.TypeLits                   ( KnownSymbol
                                                , symbolVal
                                                )
import           Lens.Micro                     ( over )
import           Network.HTTP.Client            ( queryString )
import           Network.HTTP.Types             ( parseQueryText
                                                , queryTextToQuery
                                                , renderQuery
                                                )
import           Network.Wai                    ( rawQueryString )
import           Servant
import           Servant.Docs                   ( DocQueryParam(..)
                                                , HasDocs(..)
                                                , ParamKind(..)
                                                , ToParam(..)
                                                , params
                                                )
import           Servant.Ekg                    ( HasEndpoint(..) )
import qualified Servant.Foreign               as Foreign
import           Servant.Foreign                ( Arg(..)
                                                , HasForeignType(..)
                                                , NoContent
                                                , NoTypes
                                                , PathSegment(..)
                                                , QueryArg(..)
                                                )
import           Servant.QuickCheck.Internal.HasGenRequest
                                                ( HasGenRequest(..) )
import           Servant.Server.Internal.Delayed
                                                ( addParameterCheck )
import           Servant.Server.Internal.DelayedIO
                                                ( delayedFailFatal
                                                , withRequest
                                                )
import           Servant.Swagger.Internal       ( HasSwagger(..)
                                                , addDefaultResponse400
                                                , addParam
                                                )
import           Test.QuickCheck                ( Arbitrary
                                                , Gen
                                                , arbitrary
                                                )

-- | Get the foreign query argument information
toQueryArg :: ftype -> DocQueryParam -> QueryArg ftype
toQueryArg ftype (DocQueryParam n _ _ k) = QueryArg
  { _queryArgName = Arg { _argName = PathSegment (Text.pack n)
                        , _argType = ftype
                        }
  , _queryArgType = argType k
  }
 where
  argType :: ParamKind -> Foreign.ArgType
  argType Normal = Foreign.Normal
  argType List   = Foreign.List
  argType Flag   = Foreign.Flag

-- | Add a prefix to the parameter name of 'DocQueryParam'
addDocPrefix :: Text -> DocQueryParam -> DocQueryParam
addDocPrefix t doc@(DocQueryParam x1 x2 x3 x4)
  | Text.null t = doc
  | otherwise   = DocQueryParam (Text.unpack t <> "." <> x1) x2 x3 x4

-- | Like 'ToParam' but for multiple parameters
--
-- You probably want to derive this using generics, or use 'toParams1' for unary types.
-- The generic instance needs every inner most type to be an instance of 'ToParams' itself.
class ToParams lang ftype t where
  -- | Given a prefix and the type create the documentation
  toParams :: Text -> Proxy lang -> Proxy ftype -> Proxy t -> [( DocQueryParam, ParamSchema a, ftype) ]
  default toParams :: (Generic t, GToParams lang ftype (Rep t)) => Text -> Proxy lang -> Proxy ftype -> Proxy t -> [( DocQueryParam, ParamSchema a, ftype)]
  toParams = defaultToParams defaultOptions

-- | Helper to derive 'ToParam' for regular types
toParams1
  :: (ToParam t, ToParamSchema inner, HasForeignType lang ftype inner)
  => Proxy inner
  -> Text
  -> Proxy lang
  -> Proxy ftype
  -> Proxy t
  -> [(DocQueryParam, ParamSchema a, ftype)]
toParams1 inner t lang ftype proxy = [(doc, toParamSchema inner, typ)]
 where
  doc = addDocPrefix t (toParam proxy)
  typ = typeFor lang ftype inner


-- | Runs 'gToQueryText' with the empty state
defaultToParams
  :: (Generic a, GToParams lang ftype (Rep a))
  => Options
  -> Text
  -> Proxy lang
  -> Proxy ftype
  -> Proxy a
  -> [(DocQueryParam, ParamSchema t, ftype)]
defaultToParams opts pref lang ftype p =
  gToParams (emptyState pref) opts lang ftype (from <$> p)

-- | Generic helper for instances of 'ToParam'
class GToParams lang ftype (a :: * -> * ) where
  gToParams :: State
    -> Options
    -> Proxy lang
    -> Proxy ftype
    -> Proxy (a p) -- ^ Data type
    -> [( DocQueryParam, ParamSchema t, ftype)]  -- ^ List of query parameter info

-- | Print unary data types
instance {-# OVERLAPPING #-} (GToParams lang ftype f, Constructor c)
  => GToParams lang ftype (D1 t (C1 c f)) where
  gToParams s opts lang ftype p =
    gToParams (mapS opts s) opts lang ftype (unM1 <$> p)

-- | Discard data type meta information for all other data types
instance GToParams lang ftype f
  => GToParams lang ftype (D1 t f) where
  gToParams s opts lang ftype p = gToParams s opts lang ftype (unM1 <$> p)

-- | Add the constructor name, optionally ignore unaryConstructors
instance {-# OVERLAPPING #-} (GToParams lang ftype f, Constructor c, Selector s)
  => GToParams lang ftype (C1 c (S1 s f)) where
  gToParams s opts lang ftype p = gToParams (mapS opts s')
                                            opts
                                            lang
                                            ftype
                                            (unM1 <$> p)
    where s' = addCPrefix (undefined :: C1 c f ()) opts s

-- | Add the constructor name to the list of path segments for all other constructors
instance (GToParams lang ftype f, Constructor c)
  => GToParams lang ftype (C1 c f) where
  gToParams s opts lang ftype p = gToParams s' opts lang ftype (unM1 <$> p)
    where s' = addCPrefix (undefined :: C1 c f ()) opts s

-- | Add selector to the list of prefixes
instance (GToParams lang ftype f, Selector s)
  => GToParams lang ftype (S1 s f) where
  gToParams s opts lang ftype p = gToParams s' opts lang ftype (unM1 <$> p)
    where s' = addSPrefix (undefined :: S1 s f ()) opts s

-- | For products we just print both
instance (GToParams lang ftype f, GToParams lang ftype g)
  => GToParams lang ftype (f :*: g) where
  gToParams s opts lang ftype Proxy =
    gToParams s opts lang ftype (Proxy :: Proxy (f p))
      ++ gToParams s opts lang ftype (Proxy :: Proxy (g p))

-- | For sums we print either one of them
instance (GToParams lang ftype f, GToParams lang ftype g)
  => GToParams lang ftype (f :+: g) where
  gToParams s opts lang ftype Proxy =
    gToParams s opts lang ftype (Proxy :: Proxy (f p))
      ++ gToParams s opts lang ftype (Proxy :: Proxy (g p))

-- | This is a wrapper for the inner most datatype.
instance ToParams lang ftype c => GToParams lang ftype (K1 i c) where
  gToParams s _opts lang ftype Proxy =
    toParams (toParamName s) lang ftype (Proxy :: Proxy c)

-- INSTANCES

-- | 'HasServer' instance for 'QueryObject'
instance (HasServer api context, FromQueryText a, KnownSymbol sym)
    => HasServer (QueryObject sym a :> api) context where
  type ServerT (QueryObject sym a :> api) m = a -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context
      $                   subserver
      `addParameterCheck` withRequest (paramsCheck paramname)
   where
    paramname = Text.pack $ symbolVal (Proxy :: Proxy sym)
    querytext req = parseQueryText $ rawQueryString req
    paramsCheck proxy req = case fromQueryText proxy (querytext req) of
      ParseError err -> delayedFailFatal err400
        { errBody = "Error parsing query parameter(s): "
                      <> Text.encodeUtf8 (Text.fromStrict err)
        }
      Found  parsed -> return parsed
      Absent parsed -> return parsed


instance (HasDocs api, ToParams NoTypes NoContent a, KnownSymbol sym)
    => HasDocs ( QueryObject sym a :> api ) where

  docsFor Proxy (endpoint, action) = docsFor subApiP (endpoint, action')

   where
    paramname = symbolVal (Proxy :: Proxy sym)
    subApiP   = Proxy :: Proxy api
    paramP    = Proxy :: Proxy a
    action'   = over
      params
      (++ map
        (\(x, _, _) -> x)
        (toParams (Text.pack paramname)
                  (Proxy :: Proxy NoTypes)
                  (Proxy :: Proxy NoContent)
                  paramP
        )
      )
      action


instance (HasSwagger api, ToParams NoTypes NoContent a, KnownSymbol sym)
    => HasSwagger (QueryObject sym a :> api) where
  toSwagger _ = foldl'
    addDocQueryParam
    (toSwagger (Proxy :: Proxy api))
    (toParams paramname
              (Proxy :: Proxy NoTypes)
              (Proxy :: Proxy NoContent)
              (Proxy :: Proxy a)
    )
   where
    paramname = Text.pack $ symbolVal (Proxy :: Proxy sym)
    addDocQueryParam swagger (p, psch, _) =
      addDefaultResponse400 (Text.pack (_paramName p))
      . addParam param
      $ swagger
     where
        -- Parameters in the QueryObject should never be require
      param = Param (Text.pack (_paramName p))
                    (Just . Text.pack $ _paramDesc p)
                    (Just False)
                    (ParamOther sch)
      sch = ParamOtherSchema ParamQuery (Just True) psch


instance (HasGenRequest api, Arbitrary a, ToQueryText a, KnownSymbol sym)
    => HasGenRequest (QueryObject sym a :> api) where
  genRequest _ =
    ( oldf
    , do
      new' <- new
      old' <- old
      return $ \burl ->
        let r       = old' burl
            newExpr = toQueryString new'
            qs      = queryString r
        in  r { queryString = if qs == "" then newExpr else newExpr <> "&" <> qs
              }
    )
   where
    (oldf, old) = genRequest (Proxy :: Proxy api)
    paramname   = Text.pack $ symbolVal (Proxy :: Proxy sym)
    new         = arbitrary :: Gen a
    toQueryString =
      renderQuery False . queryTextToQuery . toQueryText paramname

instance HasEndpoint api
    => HasEndpoint (QueryObject sym a :> api) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy api)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy api)

instance ToParamSchema a => ToParamSchema (Maybe a) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy a)
