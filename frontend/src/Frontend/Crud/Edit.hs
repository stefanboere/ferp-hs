{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Crud.Edit
  ( dynWidget
  , getButton
  , deleteButton
  , postButton
  , patchButton
  , createPatchDyn
  , withLocalUndoRedo
  , editPrimaryKey
  , applyPkFromPostResponse
  , EditFormConfig(..)
  , editForm
  )
where

import           Control.Lens
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Functor.Compose
import           Data.Text                      ( Text )
import           Reflex.Dom              hiding ( Link )
import qualified Reflex.Dom.Prerender          as Prerender
                                                ( Client )
import           Servant.API             hiding ( URI(..) )
import           Servant.Crud.API               ( LocationHdr )
import           Servant.Crud.QueryOperator     ( MaybeLast(..) )
import           Servant.Links           hiding ( URI(..) )
import           URI.ByteString                 ( URI )

import           Common.Schema
import           Components
import           Frontend.Context               ( AppT )
import           Frontend.Crud.Utils




-- | Reconstruct an event creating widget and flaten the result.
-- Works well with conditionally visible buttons, such as @getButton@.
dynWidget
  :: (Adjustable t m, MonadHold t m)
  => a
  -> Event t a
  -> (a -> m (Event t b))
  -> m (Event t b)
dynWidget initVal setEv constr =
  switchDyn <$> widgetHold (constr initVal) (constr <$> setEv)

-- | Create a load button, which performs the load request on build.
-- Is hidden when the primary key is Nothing.
getButton
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => (c -> Request (Prerender.Client (AppT t m)) b) -- ^ The get request
  -> Maybe c -- ^ The primary key
  -> AppT t m (Event t (Response (Prerender.Client (AppT t m)) b))
getButton _   Nothing   = pure never
getButton req (Just pk) = do
  autoEv <- getPostBuild
  requestBtn refreshBtn
             (pure . (req pk <$))
             (constDyn False)
             (constDyn False)
             autoEv

-- | Create a delete button, which performs the delete request after confirmation.
-- Is hidden if the primary key is Nothing.
deleteButton
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => (c -> Request (Prerender.Client (AppT t m)) b) -- ^ The delete request
  -> Maybe c -- ^ The primary key
  -> AppT t m (Event t (Response (Prerender.Client (AppT t m)) b))
deleteButton _   Nothing   = pure never
deleteButton req (Just pk) = requestBtn deleteBtn
                                        deleteReq
                                        (constDyn False)
                                        (constDyn False)
                                        never
 where
  deleteReq ev = do
    ev' <- deleteConfirmation (const 1) ev
    pure $ req pk <$ ev'

-- | Create a save button, which performs the request if the dynamic is Just.
-- Is hidden if the primary key is Just
postButton
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , Eq a
     )
  => (a -> Request (Prerender.Client (AppT t m)) b) -- ^ The insert request
  -> Dynamic t (Maybe a) -- ^ The form value
  -> Maybe c -- ^ The primary key
  -> AppT t m (Event t (Response (Prerender.Client (AppT t m)) b))
postButton _   _      (Just _) = pure never
postButton req dynRec Nothing  = requestBtn saveBtn
                                            (pure . postReq)
                                            (constDyn False)
                                            ((== Nothing) <$> dynRec)
                                            never
  where postReq ev = attachPromptlyDynWithMaybe (\x () -> fmap req x) dynRec ev


-- | Create a save button, which performs the request if the dynamic is Just and not empty
-- Is hidden if the primary key is Nothing
patchButton
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , Eq a
     , Monoid a
     )
  => (c -> a -> Request (Prerender.Client (AppT t m)) b) -- ^ The patch request
  -> Dynamic t (Maybe a) -- ^ The form value
  -> Event t () -- ^ The auto save event
  -> Maybe c -- ^ The primary key
  -> AppT
       t
       m
       (Event t (Response (Prerender.Client (AppT t m)) b))
patchButton _   _        _       Nothing   = pure never
patchButton req dynPatch patchEv (Just pk) = requestBtn
  saveBtn
  (pure . patchReq)
  ((Just mempty ==) <$> dynPatch)
  ((Nothing ==) <$> dynPatch)
  patchEv
 where
  patchReq ev = attachPromptlyDynWithMaybe (\x () -> req pk <$> x) dynPatch ev

-- | If the edited value differs from the remote value,
-- create an event to apply these changes after 3 seconds.
createPatchDyn
  :: ( Beamable r
     , FieldsFulfillConstraint Eq r
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     , MonadFix m
     , MonadIO (Performable m)
     , Eq (r MaybeLast)
     , Monoid (r MaybeLast)
     )
  => Dynamic t (Maybe (r Identity)) -- ^ The currently saved value
  -> Dynamic t (Maybe (r Identity)) -- ^ The edited value
  -> m (Dynamic t (Maybe (r MaybeLast)), Event t ()) -- ^ The patch and debounced update event
createPatchDyn dynRemote dynRec = do
  let dynPatch = makePatch' <$> dynRemote <*> dynRec
  patchEv <- debounce 3 (() <$ fmapMaybe validAndNonempty (updated dynPatch))
  pure (dynPatch, patchEv)

 where
  makePatch' x my = makePatch <$> x <*> my
  validAndNonempty (Just x) | x == mempty = Nothing
                            | otherwise   = Just x
  validAndNonempty Nothing = Nothing

-- | Creates undo and redo buttons.
-- The incoming events are not stored in the undo redo list.
-- The result is the incoming events combined with the undo redo events
withLocalUndoRedo
  :: ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadHold t m
     , MonadFix m
     , MonadIO (Performable m)
     , Eq a
     , Monoid a
     )
  => Event t a
  -> Dynamic t a
  -> m (Event t a)
withLocalUndoRedo remoteEv dynRec = do
  uniqDynRec       <- holdUniqDyn dynRec
  debounceDynRecEv <- debounce 1 (difference (updated uniqDynRec) remoteEv)
  let debounceDynRecEvValid = ffilter (/= mempty) debounceDynRecEv
  (hEv, tEv) <- headTailE debounceDynRecEvValid
  dynUndoEv  <- widgetHold (pure never) ((`undoRedo` tEv) <$> hEv)
  pure $ leftmost [remoteEv, switchDyn dynUndoEv]

-- | Creates an editor which forces the primary key to the specified value
editPrimaryKey
  :: (Reflex t, MonadHold t m)
  => (c -> a -> a)
  -> c
  -> Event t c
  -> Compose m (Dynamic t) (a -> a)
editPrimaryKey setPk initPk setPkEv = Compose $ do
  dynPk <- holdDyn initPk setPkEv
  pure (setPk <$> dynPk)

-- | Read the primary key from the post response and update the location
applyPkFromPostResponse
  :: (TriggerEvent t m, PerformEvent t m, Prerender js t m)
  => (c -> Link)
  -> Event t (Headers '[LocationHdr] c)
  -> m (Event t (Maybe c))
applyPkFromPostResponse mkLnk postRespEv = do
  let reqBody = getResponse <$> postRespEv
  replaceLocation (mkLnk <$> reqBody)
  pure (Just <$> reqBody)


data EditFormConfig t m a = EditFormConfig
  { _formConfig_actions
      :: Maybe (PrimaryKey a Identity)
      -> Event t (Maybe (PrimaryKey a Identity))
      -> Dynamic t (Maybe (a Identity))
      -> Dynamic t (a MaybeLast)
      -> AppT t m (Event t URI)
  , _formConfig_getReq
      :: PrimaryKey a Identity
      -> Request (Prerender.Client (AppT t m)) (a Identity)
  , _formConfig_deleteReq
      :: PrimaryKey a Identity
      -> Request (Prerender.Client (AppT t m)) NoContent
  , _formConfig_postReq
      :: a Identity
      -> Request
           (Prerender.Client (AppT t m))
           (Headers '[LocationHdr] (PrimaryKey a Identity))
  , _formConfig_patchReq
      :: PrimaryKey a Identity
      -> a MaybeLast
      -> Request (Prerender.Client (AppT t m)) NoContent
  , _formConfig_setPrimaryKey
      :: Maybe (PrimaryKey a Identity) -> a MaybeLast -> a MaybeLast
  , _formConfig_header           :: a MaybeLast -> Text
  , _formConfig_routeAfterDelete :: Link
  , _formConfig_editRoute        :: PrimaryKey a Identity -> Link
  }


editForm
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , PerformEvent t m
     , Beamable a
     , FieldsFulfillConstraint Eq a
     , Eq (a Identity)
     , Eq (a MaybeLast)
     , Monoid (a MaybeLast)
     )
  => EditFormConfig t m a
  -> (  Event t (a MaybeLast)
     -> EventWriterT
          t
          (MaybeLast URI)
          (AppT t m)
          (Dynamic t (a MaybeLast))
     )
  -> Maybe (PrimaryKey a Identity)
  -> AppT t m (Event t URI)
editForm cfg editor initPk = do
  rec
    el "h1" $ dynText (_formConfig_header cfg <$> mDynRec)

    backBtn "Close"
    getEvResult     <- dynWidget initPk setPkEv getButton'
    patchEvResult   <- dynWidget initPk setPkEv (patchButton' dynPatch patchEv)
    postEvResult    <- dynWidget initPk setPkEv (postButton' dynRec)
    deleteEvResult  <- dynWidget initPk setPkEv deleteButton'
    modRecEv        <- withLocalUndoRedo (purePatch <$> getEvSuccess) mDynRec

    actionRouteEv <- _formConfig_actions cfg initPk setPkEv dynRecRemote mDynRec

    getEvSuccess    <- orAlert getEvResult
    _               <- orAlert patchEvResult
    deleteEvSuccess <- orAlert deleteEvResult
    postEvSuccess   <- orAlert postEvResult
    setPkEv <- applyPkFromPostResponse (_formConfig_editRoute cfg) postEvSuccess

    dynRecRemote    <- holdDyn Nothing (Just <$> getEvSuccess)

    dynPk           <- holdDyn initPk setPkEv
    (mDynRec', rEv) <- runEventWriterT $ editor modRecEv
    let mDynRec = _formConfig_setPrimaryKey cfg <$> dynPk <*> mDynRec'
    let dynRec  = joinPatch <$> mDynRec

    (dynPatch, patchEv) <- createPatchDyn dynRecRemote dynRec

  pure $ leftmost
    [ coerceUri (linkURI (_formConfig_routeAfterDelete cfg)) <$ deleteEvSuccess
    , actionRouteEv
    , fmapMaybe unMaybeLast rEv
    ]
 where
  getButton'    = getButton (_formConfig_getReq cfg)
  deleteButton' = deleteButton (_formConfig_deleteReq cfg)
  patchButton'  = patchButton (_formConfig_patchReq cfg)
  postButton'   = postButton (_formConfig_postReq cfg)
