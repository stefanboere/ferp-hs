{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Crud.Datagrid
  ( toApiPage
  , fromApiPage
  , replaceLocation
  , toApiDirection
  , fromApiDirection
  , fromApiOrdering
  ) where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           GHCJS.DOM.Types                ( SerializedScriptValue(..) )
import           Language.Javascript.JSaddle    ( pToJSVal )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import qualified Servant.Crud.API              as API
                                                ( Page(..) )
import qualified Servant.Crud.OrderBy          as API
import           Servant.Links                  ( Link
                                                , URI(..)
                                                , linkURI
                                                )

import           Components.Table



toApiPage :: Page -> API.Page
toApiPage p = API.Page
  { API.offset = if _page_num p <= 1
                   then Nothing
                   else Just (_page_size p * (_page_num p - 1))
  , API.limit  = Just (_page_size p)
  }

fromApiPage :: API.Page -> Page
fromApiPage p =
  let pagesize = fromMaybe (pageSize def) (API.limit p)
  in  Page { _page_num  = (fromMaybe 0 (API.offset p) `div` pagesize) + 1
           , _page_size = pagesize
           }

replaceLocation
  :: (TriggerEvent t m, PerformEvent t m, Prerender js t m)
  => Event t Link
  -> m ()
replaceLocation lEv = prerender_ (pure ()) $ do
  _ <- manageHistory
    (HistoryCommand_ReplaceState . historyItem . linkURI <$> lEv)
  pure ()
 where
  historyItem :: URI -> HistoryStateUpdate
  historyItem uri = HistoryStateUpdate
    { _historyStateUpdate_state = SerializedScriptValue (pToJSVal False)
    , _historyStateUpdate_title = ""
    , _historyStateUpdate_uri   = Just uri { uriPath = "/" <> uriPath uri }
    }

fromApiDirection :: API.Direction -> SortOrder
fromApiDirection API.Ascending  = Ascending
fromApiDirection API.Descending = Descending

toApiDirection :: SortOrder -> API.Direction
toApiDirection Ascending  = API.Ascending
toApiDirection Descending = API.Descending

fromApiOrdering :: [API.OrderBy c a] -> Map API.Path SortOrder
fromApiOrdering = Map.fromList . fmap
  (\x -> (API.orderByPath x, fromApiDirection (API.orderByDirection x)))
