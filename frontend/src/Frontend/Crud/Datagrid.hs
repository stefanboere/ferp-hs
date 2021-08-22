{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Crud.Datagrid
  ( toApiPage
  , fromApiPage
  , replaceLocation
  ) where

import           Data.Maybe                     ( fromMaybe )
import           GHCJS.DOM.Types                ( SerializedScriptValue(..) )
import           Language.Javascript.JSaddle    ( pToJSVal )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import qualified Servant.Crud.API              as API
                                                ( Page(..) )
import           Servant.Links                  ( Link
                                                , URI(..)
                                                , linkURI
                                                )

import           Components.Table



toApiPage :: Page -> API.Page
toApiPage p = API.Page { API.offset = Just (_page_size p * (_page_num p - 1))
                       , API.limit  = Just (_page_size p)
                       }

fromApiPage :: API.Page -> Page
fromApiPage p =
  let pagesize = fromMaybe (pageSize def) (API.limit p)
  in  Page { _page_num  = (fromMaybe 0 (API.offset p) `div` pagesize) + 1
           , _page_size = pagesize
           }

-- x :: (Symbol s, HasField s r a) => Descending -> OrderBy c r

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

