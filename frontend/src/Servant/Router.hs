{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-|
Module      : Servant.Router
Description : Write Servant APIs to be routed without a server
Copyright   : (c) Will Francher, 2016
                  Stefan Boeree, 2021
License     : BSD3
Maintainer  : s.boere@boerevlist.nl

Originaly part of https://github.com/ElvishJerricco/servant-router
-}
module Servant.Router where

import qualified Data.ByteString.Char8         as BS
import           Data.Proxy
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding
import           GHC.TypeLits
import           Network.HTTP.Types             ( decodePathSegments
                                                , queryToQueryText
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.AccessControl
import           Servant.Crud.QueryObject       ( FromQueryText(..)
                                                , ParseResult(..)
                                                , QueryObject
                                                )
import           URI.ByteString
import           Web.HttpApiData

-- | Router terminator.
-- The 'HasRouter' instance for 'View' finalizes the router.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "bookId" Int :> View
data View

instance HasLink View where
  type MkLink View a = MkLink (Get '[] ()) a
  toLink toA _ = toLink toA (Proxy :: Proxy (Get '[] ()))

-- | When routing, the router may fail to match a location.
-- Either this is an unrecoverable failure,
-- such as failing to parse a query parameter,
-- or it is recoverable by trying another path.
data RoutingError = Fail | FailFatal deriving (Show, Eq, Ord)

-- | A 'Router' contains the information necessary to execute a handler.
data Router m a where
  RChoice       ::Router m a -> Router m a -> Router m a
  RCapture      ::FromHttpApiData x => (x -> Router m a) -> Router m a
  RQueryParam   ::(FromHttpApiData x, KnownSymbol sym)
                   => Proxy sym -> (Maybe x -> Router m a) -> Router m a
  RQueryParams  ::(FromHttpApiData x, KnownSymbol sym)
                   => Proxy sym -> ([x] -> Router m a) -> Router m a
  RQueryFlag    ::KnownSymbol sym
                   => Proxy sym -> (Bool -> Router m a) -> Router m a
  RQueryObject  ::(FromQueryText x, KnownSymbol sym)
                   => Proxy sym -> (x -> Router m a) -> Router m a
  RPath         ::KnownSymbol sym => Proxy sym -> Router m a -> Router m a
  RPage         ::m a -> Router m a

-- | Transform a layout by replacing 'View' with another type
type family ViewTransform layout view where
  ViewTransform (a :<|> b) view = ViewTransform a view :<|> ViewTransform b view
  ViewTransform (a :> b) view = a :> ViewTransform b view
  ViewTransform View view = view

-- | This is similar to the @HasServer@ class from @servant-server@.
-- It is the class responsible for making API combinators routable.
-- 'RuoteT' is used to build up the handler types.
-- 'Router' is returned, to be interpretted by 'routeLoc'.
class HasRouter layout where
  -- | A route handler.
  type RouteT layout (m :: * -> *) a :: *
  -- | Create a constant route handler that returns @a@
  constHandler :: Monad m => Proxy layout -> Proxy m -> a -> RouteT layout m a
  hoistRoute :: Proxy layout -> (m a -> n b) -> RouteT layout m a -> RouteT layout n b
  -- | Transform a route handler into a 'Router'.
  route :: Proxy layout -> Proxy m -> Proxy a -> RouteT layout m a -> Router m a
  -- | Create a 'Router' from a constant.
  routeConst :: Monad m => Proxy layout -> Proxy m -> a -> Router m a
  routeConst l m a = route l m (Proxy :: Proxy a) (constHandler l m a)

instance (HasRouter x, HasRouter y) => HasRouter (x :<|> y) where
  type RouteT (x :<|> y) m a = RouteT x m a :<|> RouteT y m a
  constHandler _ m a =
    constHandler (Proxy :: Proxy x) m a :<|> constHandler (Proxy :: Proxy y) m a
  hoistRoute _ nt (a :<|> b) =
    hoistRoute (Proxy :: Proxy x) nt a :<|> hoistRoute (Proxy :: Proxy y) nt b
  route _ (m :: Proxy m) (a :: Proxy a) ((x :: RouteT x m a) :<|> (y :: RouteT
      y
      m
      a                                                                       ))
    = RChoice (route (Proxy :: Proxy x) m a x) (route (Proxy :: Proxy y) m a y)

instance (HasRouter sublayout, FromHttpApiData x)
         => HasRouter (Capture sym x :> sublayout) where
  type RouteT (Capture sym x :> sublayout) m a = x -> RouteT sublayout m a
  constHandler _ m a _ = constHandler (Proxy :: Proxy sublayout) m a
  hoistRoute _ nt s = hoistRoute (Proxy :: Proxy sublayout) nt . s
  route _ m a f = RCapture (route (Proxy :: Proxy sublayout) m a . f)

instance (HasRouter sublayout, FromHttpApiData x, KnownSymbol sym)
         => HasRouter (QueryParam sym x :> sublayout) where
  type RouteT (QueryParam sym x :> sublayout) m a
    = Maybe x -> RouteT sublayout m a
  constHandler _ m a _ = constHandler (Proxy :: Proxy sublayout) m a
  hoistRoute _ nt s = hoistRoute (Proxy :: Proxy sublayout) nt . s
  route _ m a f = RQueryParam (Proxy :: Proxy sym)
                              (route (Proxy :: Proxy sublayout) m a . f)

instance (HasRouter sublayout, FromHttpApiData x, KnownSymbol sym)
         => HasRouter (QueryParams sym x :> sublayout) where
  type RouteT (QueryParams sym x :> sublayout) m a = [x] -> RouteT sublayout m a
  constHandler _ m a _ = constHandler (Proxy :: Proxy sublayout) m a
  hoistRoute _ nt s = hoistRoute (Proxy :: Proxy sublayout) nt . s
  route _ m a f =
    RQueryParams (Proxy :: Proxy sym) (route (Proxy :: Proxy sublayout) m a . f)

instance (HasRouter sublayout, KnownSymbol sym)
         => HasRouter (QueryFlag sym :> sublayout) where
  type RouteT (QueryFlag sym :> sublayout) m a = Bool -> RouteT sublayout m a
  constHandler _ m a _ = constHandler (Proxy :: Proxy sublayout) m a
  hoistRoute _ nt s = hoistRoute (Proxy :: Proxy sublayout) nt . s
  route _ m a f =
    RQueryFlag (Proxy :: Proxy sym) (route (Proxy :: Proxy sublayout) m a . f)

instance (HasRouter sublayout, FromQueryText x, KnownSymbol sym)
         => HasRouter (QueryObject sym x :> sublayout) where
  type RouteT (QueryObject sym x :> sublayout) m a = x -> RouteT sublayout m a
  constHandler _ m a _ = constHandler (Proxy :: Proxy sublayout) m a
  hoistRoute _ nt s = hoistRoute (Proxy :: Proxy sublayout) nt . s
  route _ m a f =
    RQueryObject (Proxy :: Proxy sym) (route (Proxy :: Proxy sublayout) m a . f)

instance (HasRouter sublayout)
         => HasRouter (Auth' auths v r :> sublayout) where
  type RouteT (Auth' auths v r :> sublayout) m a = RouteT sublayout m a
  constHandler _ = constHandler (Proxy :: Proxy sublayout)
  hoistRoute _ = hoistRoute (Proxy :: Proxy sublayout)
  route _ = route (Proxy :: Proxy sublayout)

instance (HasRouter sublayout, KnownSymbol path)
         => HasRouter (path :> sublayout) where
  type RouteT (path :> sublayout) m a = RouteT sublayout m a
  constHandler _ = constHandler (Proxy :: Proxy sublayout)
  hoistRoute _ = hoistRoute (Proxy :: Proxy sublayout)
  route _ m a page =
    RPath (Proxy :: Proxy path) (route (Proxy :: Proxy sublayout) m a page)

instance HasRouter View where
  type RouteT View m a = m a
  constHandler _ _ = return
  hoistRoute _ nt = nt
  route _ _ _ = RPage

-- | Use a handler to route a 'URIRef'.
routeURI
  :: (HasRouter layout, Monad m)
  => Proxy layout
  -> RouteT layout m a
  -> URIRef uri
  -> m (Either RoutingError a)
routeURI layout page uri =
  let routing = route layout Proxy Proxy page
      toMaybeQuery (k, v) = if BS.null v then (k, Nothing) else (k, Just v)

      (path, query) = case uri of
        URI{}         -> (uriPath uri, uriQuery uri)
        RelativeRef{} -> (rrPath uri, rrQuery uri)
  in  routeQueryAndPath (toMaybeQuery <$> queryPairs query)
                        (decodePathSegments path)
                        routing

-- | Use a computed 'Router' to route a path and query. Generally,
-- you should use 'routeURI'.
routeQueryAndPath
  :: Monad m
  => [(BS.ByteString, Maybe BS.ByteString)]
  -> [Text]
  -> Router m a
  -> m (Either RoutingError a)
routeQueryAndPath queries pathSegs r = case r of
  RChoice a b -> do
    result <- routeQueryAndPath queries pathSegs a
    case result of
      Left  Fail      -> routeQueryAndPath queries pathSegs b
      Left  FailFatal -> return $ Left FailFatal
      Right x         -> return $ Right x
  RCapture f -> case pathSegs of
    []              -> return $ Left Fail
    capture : paths -> maybe (return $ Left FailFatal)
                             (routeQueryAndPath queries paths)
                             (f <$> parseUrlPieceMaybe capture)
  RQueryParam sym f -> case lookup (BS.pack $ symbolVal sym) queries of
    Nothing          -> routeQueryAndPath queries pathSegs $ f Nothing
    Just Nothing     -> return $ Left FailFatal
    Just (Just text) -> case parseQueryParamMaybe (decodeUtf8 text) of
      Nothing -> return $ Left FailFatal
      Just x  -> routeQueryAndPath queries pathSegs $ f (Just x)
  RQueryParams sym f ->
    maybe (return $ Left FailFatal) (routeQueryAndPath queries pathSegs . f)
      $ do
          ps <-
            sequence
            $   snd
            <$> filter (\(k, _) -> k == BS.pack (symbolVal sym)) queries
          sequence $ parseQueryParamMaybe . decodeUtf8 <$> ps
  RQueryFlag sym f -> case lookup (BS.pack $ symbolVal sym) queries of
    Nothing       -> routeQueryAndPath queries pathSegs $ f False
    Just Nothing  -> routeQueryAndPath queries pathSegs $ f True
    Just (Just _) -> return $ Left FailFatal
  RQueryObject sym f ->
    let paramname = T.pack $ symbolVal sym
    in  case fromQueryText paramname (queryToQueryText queries) of
          ParseError _ -> return $ Left FailFatal
          Found      p -> routeQueryAndPath queries pathSegs $ f p
          Absent     p -> routeQueryAndPath queries pathSegs $ f p
  RPath sym a -> case pathSegs of
    []        -> return $ Left Fail
    p : paths -> if p == T.pack (symbolVal sym)
      then routeQueryAndPath queries paths a
      else return $ Left Fail
  RPage a -> case pathSegs of
    [] -> Right <$> a
    _  -> return $ Left Fail
