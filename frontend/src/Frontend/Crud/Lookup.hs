{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Frontend.Crud.Lookup
  ( lookupInput
  , LabelEndpoint(..)
  , labelEndpoint
  , LookupInputConfig
  ) where

import           Control.Lens                   ( Lens' )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Functor.Const             ( Const(..) )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Text                      ( Text )
import           Reflex.Dom
import qualified Reflex.Dom.Prerender          as Prerender
                                                ( Client )
import qualified Servant.Crud.API              as API
                                                ( GetListHeaders
                                                , Page(..)
                                                , page
                                                )
import           Servant.Crud.QueryOperator     ( Filter )
import           Servant.Subscriber.Reflex      ( ApiWidget
                                                , requestingJs
                                                )

import           Common.Api                     ( View )
import           Common.Schema
import           Components.Input
import           Frontend.Crud.Datagrid
import           Frontend.Crud.Utils

type LookupInputConfig t m k = InputConfig' (LabelEndpoint t m k) t m

newtype LabelEndpoint t m k a = LabelEndpoint {
  unLabelEndpoint :: OptionsRequest k -> Request (Prerender.Client (ApiWidget t m)) (API.GetListHeaders (Elem a))
}

lookupInput
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , Prerender js t m
     , Ord (PrimaryKey a Identity)
     )
  => LookupInputConfig
       t
       (ApiWidget t m)
       (PrimaryKey a Identity)
       (Maybe (Named a Identity))
  -> ApiWidget
       t
       m
       (DomInputEl t (ApiWidget t m) (Maybe (Named a Identity)))
lookupInput cfg = do
  (x, _) <- comboboxInputKS
    (pure ())
    showOpt
    def
    requestOptions
    (fmap toCbValue cfg { _inputConfig_extra = Const () })

  pure (fmap fromCbValue x)

 where
  requestOptions ev = do
    dynReq       <- holdDyn Nothing $ Just <$> ev
    dynReqUniq   <- holdUniqDyn dynReq
    dynReqTrottl <- debounce 1 (updated dynReqUniq)
    let reqEv = endpoint <$> fmapMaybe Prelude.id dynReqTrottl
    resultEv <- requestingJs reqEv
    pure
      (   either (Left . fst . showClientError)
                 (Right . fmap toTuple . getListToMapsubset)
      <$> resultEv
      )

  toTuple n = (_id n, _name n)

  endpoint = unLabelEndpoint (_inputConfig_extra cfg)

  showOpt _ = dynText

  toCbValue
    :: Maybe (Named a Identity) -> ComboboxValue (Maybe (PrimaryKey a Identity))
  toCbValue (Just n) =
    ComboboxValue { _cb_selection = Just (_id n), _cb_text = _name n }
  toCbValue Nothing = ComboboxValue { _cb_selection = Nothing, _cb_text = "" }

  fromCbValue
    :: ComboboxValue (Maybe (PrimaryKey a Identity)) -> Maybe (Named a Identity)
  fromCbValue ComboboxValue { _cb_selection = mx, _cb_text = t } =
    (\pk -> Named { _id = pk, _name = t }) <$> mx

labelEndpoint
  :: (Monoid (a Filter))
  => Lens' (a Filter) (C Filter Text)
  -> (  View be a
     -> Request
          (Prerender.Client (ApiWidget t m))
          (API.GetListHeaders (Named a Identity))
     )
  -> LabelEndpoint
       t
       m
       (PrimaryKey a Identity)
       (Maybe (Named a Identity))
labelEndpoint l req = LabelEndpoint mkRequest
 where
  mkRequest (SearchText    t (off, lim)) = mkRequestText t off lim
  mkRequest (NearSelection _ lim       ) = mkRequestText "" (0 :: Integer) lim

  mkRequestText t off lim = req $ setContains
    l
    t
    (mempty
      { API.page = API.Page
                     { API.offset = if off <= 0
                                      then Nothing
                                      else Just (fromIntegral off)
                     , API.limit  = Just . fromIntegral $ lim
                     }
      }
    )


