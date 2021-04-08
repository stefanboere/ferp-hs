{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.AddSetHeader
  ( AddSetHeadersApi
  , AddSetHeaderApi
  , AddSetHeaderApiVerb
  , AddSetHeaders
  , addSetHeaders
  , SetHeaderList(..)
  )
where

import           Prelude

import           Data.String                    ( fromString )
import           Data.Tagged                    ( Tagged(..) )
import           GHC.TypeLits                   ( Symbol
                                                , KnownSymbol
                                                , symbolVal
                                                )
import qualified Network.HTTP.Types            as HTTP
import           Network.Wai                    ( mapResponseHeaders )
import           Servant

-- What are we doing here? Well, the idea is to add headers to the response,
-- but the headers come from the authentication check. In order to do that, we
-- tweak a little the general theme of recursing down the API tree; this time,
-- we recurse down a variation of it that adds headers to all the endpoints.
-- This involves the usual type-level checks.
--
-- TODO: If the endpoints already have headers, this will not work as is
--
-- This is coming from Servant.Auth.Server.Internal.AddSetCookie (c) Julian K. Arni
-- but also works for other headers.

-- | Add multiple headers to the entire api
type family AddSetHeadersApi (xs :: [(Symbol, *)] ) a where
  AddSetHeadersApi '[] a = a
   -- Note that this is the reverse order as in AddSetCookie
  AddSetHeadersApi ('(sym, t) ': xs) a = AddSetHeaderApi sym t (AddSetHeadersApi xs a)

-- | Add a single header with name sym and type t to a verb
type family AddSetHeaderApiVerb (sym :: Symbol) t a where
  AddSetHeaderApiVerb sym t (Headers ls a) = Headers (Header sym t ': ls) a
  AddSetHeaderApiVerb sym t a = Headers '[Header sym t] a

-- | Add a single header with name sym and type t to the entire api
type family AddSetHeaderApi (sym :: Symbol) t a :: *
type instance AddSetHeaderApi sym t (a :> b) = a :> AddSetHeaderApi sym t b
type instance AddSetHeaderApi sym t (a :<|> b)
  = AddSetHeaderApi sym t a :<|> AddSetHeaderApi sym t b
type instance AddSetHeaderApi sym t (Verb method stat ctyps a)
  = Verb method stat ctyps (AddSetHeaderApiVerb sym t a)
type instance AddSetHeaderApi sym t Raw = Raw
type instance AddSetHeaderApi sym t (Stream method stat framing ctyps a)
  = Stream method stat framing ctyps (AddSetHeaderApiVerb sym t a)
type instance AddSetHeaderApi sym t (Headers hs a)
  = AddSetHeaderApiVerb sym t (Headers hs a)

-- | A list of headers to set
data SetHeaderList (xs :: [(Symbol, *)]) :: * where
  SetHeaderNil ::SetHeaderList '[]
  SetHeaderCons ::(KnownSymbol sym, ToHttpApiData t)
    => Proxy sym -> Maybe t -> SetHeaderList xs -> SetHeaderList ('(sym, t) ': xs)

-- | Adds a list of http headers to a response
class AddSetHeaders xs orig new where
  -- | Adds a list of headers to a orig 'ServerT' type to get a new type 
  addSetHeaders :: SetHeaderList xs -> orig -> new

instance {-# OVERLAPS #-} AddSetHeaders ('(sym, t) ': xs) oldb newb
  => AddSetHeaders ('(sym, t) ': xs) (a -> oldb) (a -> newb) where
  addSetHeaders cookies oldfn = addSetHeaders cookies . oldfn

instance AddSetHeaders '[] orig orig where
  addSetHeaders _ = id

instance {-# OVERLAPPABLE #-}
  ( AddSetHeaders xs (Handler old) (Handler cookied)
  , AddHeader sym t cookied new
  ) => AddSetHeaders ('(sym, t) ': xs) (Handler old) (Handler new)  where
  addSetHeaders (SetHeaderCons _ mheader rest) oldVal = case mheader of
    Nothing     -> noHeader <$> addSetHeaders rest oldVal
    Just header -> addHeader header <$> addSetHeaders rest oldVal

instance {-# OVERLAPS #-}
  (AddSetHeaders ('(sym, t) ': xs) a a', AddSetHeaders ('(sym, t) ': xs) b b')
  => AddSetHeaders ('(sym, t) ': xs) (a :<|> b) (a' :<|> b') where
  addSetHeaders cookies (a :<|> b) =
    addSetHeaders cookies a :<|> addSetHeaders cookies b

-- | for @servant <0.11@
instance
  AddSetHeaders ('(sym, t) ': xs) Application Application where
  addSetHeaders cookies r request respond =
    r request $ respond . mapResponseHeaders (++ mkHeaders cookies)

-- | for @servant >=0.11@
instance
  AddSetHeaders ('(sym, t) ': xs) (Tagged m Application) (Tagged m Application) where
  addSetHeaders cookies r = Tagged $ \request respond ->
    unTagged r request $ respond . mapResponseHeaders (++ mkHeaders cookies)

mkHeaders :: SetHeaderList xs -> [HTTP.Header]
mkHeaders SetHeaderNil                   = []
mkHeaders (SetHeaderCons _ Nothing rest) = mkHeaders rest
mkHeaders (SetHeaderCons sym (Just y) rest) =
  (fromString (symbolVal sym), toHeader y) : mkHeaders rest
