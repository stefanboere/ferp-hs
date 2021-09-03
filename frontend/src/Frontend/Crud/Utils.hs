{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Crud.Utils
  ( -- * General
    replaceLocation
    -- * Buttons
  , triStateBtn
  , backBtn
  , saveBtn
  , refreshBtn
  , deleteBtn
  , insertBtn
  , downloadButton
  , deleteConfirmation
  , messageBox
    -- * Properties
  , Property(..)
  , editWith
    -- * Forms
  , formProp
  , respectFocus
  , editorInputConfig
  -- * Editors
  , Editor(..)
  , coerceEditor
  , nullIfMempty
  , withDefault
  , toggleEditor
  , checkboxEditor
  , textEditor
  , noEditor
  , markdownEditor
  , dateEditor
  , requiredEditor
    -- * Api
  , requestBtn
  , orAlert
  , orAlertF
  , showError
  , showClientError
  ) where

import           Control.Exception.Base         ( displayException )
import           Control.Lens                   ( Const
                                                , Lens'
                                                , set
                                                , view
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import           Data.Default
import           Data.Functor.Compose           ( Compose(..) )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Time                      ( Day )
import           GHCJS.DOM.Types                ( SerializedScriptValue(..)
                                                , pToJSVal
                                                )
import           Network.HTTP.Types
import           Reflex.Dom              hiding ( Client
                                                , Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import           Reflex.Dom.Contrib.Router      ( goBack )
import qualified Reflex.Dom.Prerender          as Prerender
                                                ( Client )
import           Servant.API                    ( toUrlPiece
                                                , uriPath
                                                , uriQuery
                                                )
import qualified Servant.Crud.OrderBy          as API
import           Servant.Crud.QueryOperator     ( MaybeLast(..) )
import qualified Servant.Links                 as L
                                                ( Link
                                                , URI(..)
                                                , linkURI
                                                )
import qualified Servant.Subscriber.Reflex     as Sub
import           Servant.Subscriber.Reflex      ( ApiWidget
                                                , ClientError(..)
                                                )
import           URI.ByteString                 ( URI )



import           Common.Api                     ( Be
                                                , ViewOrderBy
                                                )
import           Common.Schema                  ( C )
import           Components
import           Reflex.Dom.Ace                 ( AceConfig )

replaceLocation
  :: (TriggerEvent t m, PerformEvent t m, Prerender js t m)
  => Event t L.Link
  -> m ()
replaceLocation lEv = replaceLocationUri $ L.linkURI <$> lEv

replaceLocationUri
  :: (TriggerEvent t m, PerformEvent t m, Prerender js t m)
  => Event t L.URI
  -> m ()
replaceLocationUri lEv = prerender_ (pure ()) $ do
  _ <- manageHistory (HistoryCommand_ReplaceState . historyItem <$> lEv)
  pure ()
 where
  historyItem :: L.URI -> HistoryStateUpdate
  historyItem uri = HistoryStateUpdate
    { _historyStateUpdate_state = SerializedScriptValue (pToJSVal False)
    , _historyStateUpdate_title = ""
    , _historyStateUpdate_uri   = Just uri { L.uriPath = "/" <> L.uriPath uri }
    }


triStateBtn
  :: (PostBuild t m, DomBuilder t m)
  => m ()
  -> (Text, Text, Text)
  -> Dynamic t ActionState
  -> m (Event t ())
triStateBtn ico (x, xing, xed) stateDyn =
  btn def { _buttonConfig_state    = stateDyn
          , _buttonConfig_priority = ButtonSecondary
          }
    $  icon def ico
    >> el "span" (dynText (stateText <$> stateDyn))
 where
  stateText ActionAvailable = x
  stateText ActionError     = x
  stateText ActionLoading   = xing
  stateText ActionSuccess   = xed
  stateText ActionDisabled  = xed

backBtn :: (PostBuild t m, DomBuilder t m, Prerender js t m) => Text -> m ()
backBtn lbl = do
  ev <-
    btn def { _buttonConfig_priority = ButtonSecondary }
    $  icon def timesIcon
    >> el "span" (text lbl)
  prerender_ (pure ()) $ performEvent_ (goBack <$ ev)


saveBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
saveBtn = triStateBtn floppyIcon ("Save", "Saving", "Saved")

refreshBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
refreshBtn = triStateBtn refreshIcon ("Reload", "Loading", "Loaded")

insertBtn
  :: (DomBuilder t m, PostBuild t m) => Dynamic t L.Link -> m (Event t URI)
insertBtn lnk = do
  clickEv <- ahrefPreventDefault
    (("/" <>) . toUrlPiece <$> lnk)
    (constDyn False)
    (Map.singleton "class" "button secondary")
    (icon def plusIcon >> el "span" (text "Insert"))
  pure $ tagPromptlyDyn (coerceUri . L.linkURI <$> lnk) clickEv

downloadButton :: (DomBuilder t m, PostBuild t m) => Dynamic t L.Link -> m ()
downloadButton lnk = elDynAttr
  "a"
  (attrs <$> dynHref)
  (icon def downloadIcon >> el "span" (text "Download"))
 where
  dynHref = pack . show . modUri . L.linkURI <$> lnk
  attrs h =
    Map.singleton "download" ""
      <> Map.singleton "class" "button secondary"
      <> Map.singleton "href" h
  modUri u = u
    { uriPath  = "/api/" <> uriPath u
    , uriQuery = if null (uriQuery u) || uriQuery u == "?"
                   then "?_accept=text/csv"
                   else uriQuery u <> "&_accept=text%2Fcsv"
    }


deleteBtn
  :: (PostBuild t m, DomBuilder t m) => Dynamic t ActionState -> m (Event t ())
deleteBtn = triStateBtn trashIcon ("Delete", "Deleting", "Delete")

deleteConfirmation
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => (a -> Int)
  -> Event t a
  -> m (Event t a)
deleteConfirmation sz = messageBox
  "Confirm deletion"
  (getMsg . sz)
  (btn def { _buttonConfig_priority = ButtonPrimary Danger } (text "Delete"))

 where
  getMsg 1 = "Are you sure you want to delete this item?"
  getMsg l =
    "Are you sure you want to delete these " <> pack (show l) <> " items?"

messageBox
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => Text
  -> (a -> Text)
  -> m (Event t ())
  -> Event t a
  -> m (Event t a)
messageBox titl msg okBtn openEv = fmapMaybe id
  <$> modal ModalMedium (modalContent <$> openEv)
 where
  modalContent y = card $ do
    x <- cardHeader (text titl >> modalCloseBtn)

    cardContent $ el "p" $ text (msg y)

    cardFooter $ do
      cancelEv <- cardAction "Cancel"
      okEv     <- okBtn
      pure $ leftmost [Nothing <$ x, Nothing <$ cancelEv, Just y <$ okEv]


data Property a b = Property
  { _prop_label   :: Text
  , _prop_lens    :: forall f . Lens' (a f) (C f b)
  , _prop_key     :: API.Path
  , _prop_orderBy :: SortOrder -> ViewOrderBy Be a
  }

editWith
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadIO m
     , MonadFix m
     , Functor c
     , Monoid (a MaybeLast)
     )
  => Editor c t m (Maybe b)
  -> Property a b
  -> Event t (a MaybeLast)
  -> Compose m (Dynamic t) (a MaybeLast -> a MaybeLast)
editWith e' prp = formProp e (_prop_label prp) (_prop_lens prp) mempty
  where e = coerceEditor MaybeLast unMaybeLast e'


data Editor c t m b = Editor
  { _edit_editor      :: InputConfig' c t m b -> m (DomInputEl t m b)
  , _edit_viewer      :: Dynamic t b -> m ()
  , _edit_extraConfig :: c b
  }

coerceEditor
  :: (Reflex t, Functor c, Functor m)
  => (b -> a)
  -> (a -> b)
  -> Editor c t m b
  -> Editor c t m a
coerceEditor fba fab e = e
  { _edit_editor      = fmap (fmap fba) . _edit_editor e . fmap fab
  , _edit_viewer      = \dynVal -> _edit_viewer e (fab <$> dynVal)
  , _edit_extraConfig = fmap fba (_edit_extraConfig e)
  }

-- | Converts an editor into an editor which accounts for focus
--
-- 1. If the user is focussed, no external set value events are applied
-- 2. The output dynamic is only updated on lose focus
respectFocus
  :: (DomBuilder t m, MonadFix m)
  => (InputConfig' c t m b -> m (DomInputEl t m b))
  -> InputConfig' c t m b
  -> m (DomInputEl t m b)
respectFocus editor cfg = do
  rec r <- editor cfg
        { _inputConfig_setValue = gate
                                    (current
                                      (Prelude.not <$> _inputEl_hasFocus r)
                                    )
                                    (_inputConfig_setValue cfg)
        }

  pure r

formProp
  :: (DomBuilder t m, PostBuild t m, MonadIO m, MonadFix m)
  => Editor c t m b
  -> Text
  -> Lens' a b
  -> a
  -> Event t a
  -> Compose m (Dynamic t) (a -> a)
formProp prp lbl l initVal update =
  Compose $ fmap (set l) . _inputEl_value <$> labeled
    lbl
    (respectFocus (_edit_editor prp))
    (editorInputConfig prp l initVal update)

editorInputConfig
  :: (DomBuilder t m)
  => Editor c t m b
  -> Lens' a b
  -> a
  -> Event t a
  -> InputConfig' c t m b
editorInputConfig prp l initVal update =
  (inputConfig' (_edit_extraConfig prp) (view l initVal))
    { _inputConfig_setValue = view l <$> update
    }


nullIfMempty
  :: (Reflex t, Functor c, Functor m, Eq a, Monoid a)
  => Editor c t m a
  -> Editor c t m (Maybe a)
nullIfMempty = coerceEditor nullIfEmpty (fromMaybe mempty)
 where
  nullIfEmpty x | x == mempty = Nothing
                | otherwise   = Just x

withDefault
  :: (Reflex t, Functor c, Functor m)
  => a
  -> Editor c t m a
  -> Editor c t m (Maybe a)
withDefault d = coerceEditor Just (fromMaybe d)

textEditor
  :: (DomBuilder t m, PostBuild t m) => Editor (Const ()) t m (Maybe Text)
textEditor = nullIfMempty $ Editor { _edit_editor      = textInput
                                   , _edit_viewer      = dynText
                                   , _edit_extraConfig = def
                                   }

checkboxEditor
  :: (DomBuilder t m, PostBuild t m, MonadIO m)
  => Editor (Const ()) t m (Maybe Bool)
checkboxEditor = withDefault False $ Editor { _edit_editor = checkboxInput ""
                                            , _edit_viewer = display
                                            , _edit_extraConfig = def
                                            }

toggleEditor
  :: (DomBuilder t m, PostBuild t m, MonadIO m)
  => Editor (Const ()) t m (Maybe Bool)
toggleEditor = withDefault False $ Editor { _edit_editor      = toggleInput ""
                                          , _edit_viewer      = display
                                          , _edit_extraConfig = def
                                          }

markdownEditor
  :: ( DomBuilder t m
     , PostBuild t m
     , TriggerEvent t m
     , MonadHold t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , MonadFix m
     , Prerender js t m
     )
  => Editor (Const AceConfig) t m (Maybe Text)
markdownEditor = nullIfMempty $ Editor { _edit_editor      = markdownInput
                                       , _edit_viewer      = dynText
                                       , _edit_extraConfig = cdnAceConfig
                                       }

noEditor
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, Show a)
  => Editor (Const ()) t m (Maybe a)
noEditor = Editor { _edit_editor      = noInput
                  , _edit_viewer      = display
                  , _edit_extraConfig = def
                  }

dateEditor
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Editor (NumberRange t) t m (Maybe Day)
dateEditor = Editor { _edit_editor      = dateInput
                    , _edit_viewer      = display
                    , _edit_extraConfig = def
                    }

requiredEditor
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Editor c t m (Maybe a)
  -> Editor c t m (Maybe a)
requiredEditor e = e { _edit_editor = requiredInput (_edit_editor e) }


orAlertF
  :: ( Prerender js t m
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => m (Event t (Either ClientError a))
  -> m (Event t a)
orAlertF getResultEv = do
  resultEv <- getResultEv
  let (errEv, rEv) = fanEither resultEv
  alerts def { _alertConfig_status = Danger } (showError <$> errEv)
  pure rEv

showError :: (Applicative m, Prerender js t m) => ClientError -> (Text, m ())
showError =
  fmap (\statusI -> if statusI == Just 403 then reloadAction else pure ())
    . showClientError

showClientError :: ClientError -> (Text, Maybe Int)
showClientError (FailureResponse rq rsp) =
  let status  = Sub.responseStatusCode rsp
      statusI = statusCode status
      msg =
        Text.unlines
          . catMaybes
          $ [ Just
            $  "The request to "
            <> showUrl (Sub.requestPath rq)
            <> " failed. The server responded with "
            <> (Text.pack . show $ statusI)
            <> " "
            <> (Text.decodeUtf8 . statusMessage $ status)
            <> "."
            , if BL.null (Sub.responseBody rsp)
              then Nothing
              else Just $ Text.decodeUtf8 (BL.toStrict $ Sub.responseBody rsp)
            , if statusI == 403
              then Just "Please try reloading this page."
              else Nothing
            ]
  in  (msg, Just statusI)

showClientError (DecodeFailure x _) =
  ("The response decoding failed: " <> x, Nothing)
showClientError (UnsupportedContentType x _) =
  ("The content type " <> Text.pack (show x) <> " is not supported.", Nothing)
showClientError (InvalidContentTypeHeader _) =
  ("The content type header of the response is invalid.", Nothing)
showClientError (ConnectionError e) =
  ("A connection error occured: " <> Text.pack (displayException e), Nothing)

showUrl :: (Sub.BaseUrl, ByteString) -> Text
showUrl (_, p) = Text.decodeUtf8 p

reloadAction :: (Applicative m, Prerender js t m) => m ()
reloadAction = prerender_ (pure ()) $ do
  loc <- getLocationAfterHost
  elAttr "a" ("href" =: loc) (text "Reload page")

orAlert
  :: ( Prerender js t m
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t (Either ClientError a)
  -> m (Event t a)
orAlert resultEv = orAlertF (pure resultEv)

requestBtn
  :: ( DomBuilder t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => (Dynamic t ActionState -> ApiWidget t m (Event t ()))
  -> (  Event t ()
     -> ApiWidget
          t
          m
          (Event t (Request (Prerender.Client (ApiWidget t m)) a))
     )
  -> Dynamic t Bool
  -> Dynamic t Bool
  -> Event t ()
  -> ApiWidget
       t
       m
       ( Event
           t
           (Response (Prerender.Client (ApiWidget t m)) a)
       )
requestBtn mkBtn req dynDisabled dynError reqEvAuto = do

  rec dynState <- holdDyn def $ leftmost
        [ ActionLoading <$ reqEv
        , responseState <$> resultEv
        , def <$ resultEvDelay
        ]

      reqEvMan <- mkBtn
        ((disabledState <$> dynDisabled <*> dynError) <> dynState)
      let reqEv' = leftmost [reqEvAuto, reqEvMan]

      reqEv         <- req reqEv'

      resultEv      <- Sub.requestingJs reqEv

      resultEvDelay <- debounce 2 resultEv

  pure resultEv
 where
  disabledState _    True = ActionError
  disabledState True _    = ActionDisabled
  disabledState _    _    = ActionAvailable

  responseState (Right _) = ActionSuccess
  responseState _         = ActionError

