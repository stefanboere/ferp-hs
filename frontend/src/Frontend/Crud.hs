{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Crud
  ( crudHandler
  , crudLinks
  , CrudApi
  )
where

import           Control.Applicative            ( (<**>) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Functor.Identity          ( Identity )
import           Data.Functor.Compose
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Last(..) )
import           Data.Proxy
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Crud.QueryObject       ( QObj )
import           Servant.Crud.QueryOperator     ( Filter )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router
import           Servant.Subscriber.Reflex
import           URI.ByteString                 ( URI )

import qualified Common.Api                    as Api
import           Common.Auth
import           Common.Schema
import           Components
import           Frontend.Api
import           Frontend.Context               ( AuthUser
                                                , getUserNow
                                                , AppConfig
                                                )
import           Frontend.Crud.Datagrid
import           Frontend.Crud.Edit
import           Frontend.Crud.Lookup
import           Frontend.Crud.Utils

-- brittany-disable-next-binding
type CrudApi =
  ("blogs" :>
     (    Auth Everyone :> "all" :> QObj (Api.View Api.Be BlogN) :> View
     :<|> Auth Everyone :> "new" :> View
     :<|> Auth Everyone :> Capture "key" BlogNId :> View
     )
  ) :<|>
  ("channels" :>
     (    Auth Everyone :> "all" :> QObj (Api.View Api.Be ChannelT) :> View
     :<|> Auth Everyone :> "new" :> View
     :<|> Auth Everyone :> Capture "key" ChannelId :> View
     )
  )

crudApi :: Proxy CrudApi
crudApi = Proxy

blogsLink :: AuthUser -> Api.View Api.Be BlogN -> Link
newBlogLink :: AuthUser -> Link
blogLink :: AuthUser -> BlogNId -> Link

channelsLink :: AuthUser -> Api.View Api.Be ChannelT -> Link
newChannelLink :: AuthUser -> Link
channelLink :: AuthUser -> ChannelId -> Link

-- brittany-disable-next-binding
(blogsLink :<|> newBlogLink :<|> blogLink) :<|>
  (channelsLink :<|> newChannelLink :<|> channelLink)
  = allLinks crudApi

crudLinks
  :: ( MonadFix m
     , MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     , MonadReader (AppConfig t) m
     )
  => Dynamic t URI
  -> m (Event t Link)
crudLinks dynUri = safelinkGroupAuth
  (text "Crud")
  [ safelinkAuth dynUri (`blogsLink` mempty) $ text "Blogs"
  , safelinkAuth dynUri (`channelsLink` mempty) $ text "Channels"
  ]

crudHandler
  :: WidgetConstraint js t m => RouteT CrudApi (AppT t m) (Event t URI)
crudHandler =
  (blogsHandler :<|> blogEdit Nothing :<|> (blogEdit . Just))
    :<|> (channelsHandler :<|> channelEdit Nothing :<|> (channelEdit . Just))

safelinkAuth
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadReader (AppConfig t) m
     )
  => Dynamic t URI
  -> (AuthUser -> Link)
  -> m ()
  -> m (Dynamic t Bool, Event t Link, Dynamic t Bool)
safelinkAuth dynLoc mkLnk cnt = do
  usr <- asks getUserNow
  safelinkAuth' usr dynLoc mkLnk cnt

browseFormAuth
  :: ( WidgetConstraint js t m
     , Table a
     , Ord (PrimaryKey a Identity)
     , Eq (a Filter)
     )
  => BrowseFormConfig t m AuthUser a c
  -> Api.View Api.Be a
  -> AppT t m (Event t URI)
browseFormAuth cfg vw = do
  usr <- asks getUserNow
  browseForm usr cfg vw

editFormAuth
  :: ( WidgetConstraint js t m
     , Beamable a
     , FieldsFulfillConstraint Eq a
     , Eq (a Identity)
     , Eq (a Last)
     , Monoid (a Last)
     )
  => EditFormConfig t m AuthUser a
  -> (  Dynamic t (Maybe AuthUser)
     -> Event t (a Last)
     -> EventWriterT t (Last URI) (AppT t m) (Dynamic t (a Last))
     )
  -> Maybe (PrimaryKey a Identity)
  -> AppT t m (Event t URI)
editFormAuth cfg editor initPk = do
  usr <- asks getUserNow
  editForm usr cfg (editor usr) initPk

blogsHandler
  :: (WidgetConstraint js t m)
  => Api.View Api.Be BlogN
  -> AppT t m (Event t URI)
blogsHandler = browseFormAuth BrowseFormConfig
  { _browseConfig_actions       = downloadButtonWithSelection blogId
                                                              unBlogId
                                                              getBlogsApiLink
  , _browseConfig_alerts        = const (pure never)
  , _browseConfig_getListReq    = getBlogs
  , _browseConfig_deleteListReq = deleteBlogs usingCookie
  , _browseConfig_header        = "Blogs"
  , _browseConfig_insertRoute   = newBlogLink
  , _browseConfig_editRoute     = blogLink
  , _browseConfig_browseRoute   = blogsLink
  , _browseConfig_columns = [ gridProp noEditor   eqFilter  blogIdProp
                            , gridProp textEditor strFilter blogTitleProp
                            , gridProp textEditor strFilter blogDescriptionProp
                            , gridFkProp blogChannelProp
                            ]
  }
  where unBlogId (BlogId x) = x

blogEdit :: WidgetConstraint js t m => Maybe BlogNId -> AppT t m (Event t URI)
blogEdit = editFormAuth cfg $ \usr modBlogEv ->
  el "form"
    $    getCompose
    $    pure mempty
    <**> editWith textEditor blogTitleProp modBlogEv
    <**> editFk usr blogChannelProp modBlogEv
    <**> editWith checkboxEditor   blogIsExtraProp     modBlogEv
    <**> editWith toggleEditor     blogIsPublishedProp modBlogEv
    <**> editWith (req dateEditor) blogDateProp        modBlogEv
    <**> editWith markdownEditor   blogDescriptionProp modBlogEv
 where
  cfg = EditFormConfig
    { _formConfig_actions          = \_ _ _ _ -> pure never
    , _formConfig_getReq           = getBlog
    , _formConfig_deleteReq        = deleteBlog usingCookie . coerce
    , _formConfig_postReq          = fmap (fmap coerce)
                                     . postBlog usingCookie
                                     . flattenNamed
    , _formConfig_patchReq         = \pk ->
                                       patchBlog usingCookie (coerce pk) . flattenNamed
    , _formConfig_setPrimaryKey    = setBlogPk
    , _formConfig_header           = mkHeader
    , _formConfig_routeAfterDelete = (`blogsLink` mempty)
    , _formConfig_editRoute        = blogLink
    }
  req = requiredEditor

  coerce (BlogId x) = BlogId x

  setBlogPk (Just (BlogId pk)) x = x { _blogId = pure pk }
  setBlogPk Nothing            x = x { _blogId = pure 0 }

  mkHeader x
    | fromMaybe 0 (getLast (_blogId x)) > 0 = "Blog - "
    <> fromMaybe "?" (getLast $ _blogName x)
    | otherwise = "New blog"

blogIdProp :: Property BlogN SerialInt64
blogIdProp = prop "Id" blogId (Proxy :: Proxy "_blogId")

blogChannelProp
  :: (Request (Client m) ~ FreeClient) => FkProperty t m AuthUser BlogN ChannelT
blogChannelProp = fkProp "Channel"
                         blogChannel
                         (Proxy :: Proxy "_blogChannel")
                         channelName
                         getChannelLabels
                         channelLink

blogTitleProp :: Property BlogN Text
blogTitleProp = prop "Title" blogName (Proxy :: Proxy "_blogName")

blogIsExtraProp :: Property BlogN Bool
blogIsExtraProp = prop "Extra" blogIsExtra (Proxy :: Proxy "_blogIsExtra")

blogIsPublishedProp :: Property BlogN Bool
blogIsPublishedProp =
  prop "Published" blogIsPublished (Proxy :: Proxy "_blogIsPublished")

blogDescriptionProp :: Property BlogN Text
blogDescriptionProp =
  prop "Description" blogDescription (Proxy :: Proxy "_blogDescription")

blogDateProp :: Property BlogN Day
blogDateProp = prop "Date" blogDate (Proxy :: Proxy "_blogDate")



channelsHandler
  :: (WidgetConstraint js t m)
  => Api.View Api.Be ChannelT
  -> AppT t m (Event t URI)
channelsHandler = browseFormAuth BrowseFormConfig
  { _browseConfig_actions       = downloadButtonWithSelection channelId
                                                              unChannelId
                                                              getChannelsApiLink
  , _browseConfig_alerts        = const (pure never)
  , _browseConfig_getListReq    = getChannels
  , _browseConfig_deleteListReq = deleteChannels usingCookie
  , _browseConfig_header        = "Channels"
  , _browseConfig_insertRoute   = newChannelLink
  , _browseConfig_editRoute     = channelLink
  , _browseConfig_browseRoute   = channelsLink
  , _browseConfig_columns = [ gridProp noEditor   eqFilter  channelIdProp
                            , gridProp textEditor strFilter channelNameProp
                            ]
  }
  where unChannelId (ChannelId x) = x


channelEdit
  :: WidgetConstraint js t m => Maybe ChannelId -> AppT t m (Event t URI)
channelEdit = editFormAuth cfg $ \_ modBlogEv ->
  el "form"
    $    getCompose
    $    pure mempty
    <**> editWith textEditor channelNameProp modBlogEv
 where
  cfg = EditFormConfig { _formConfig_actions          = \_ _ _ _ -> pure never
                       , _formConfig_getReq           = getChannel
                       , _formConfig_deleteReq = deleteChannel usingCookie
                       , _formConfig_postReq          = postChannel usingCookie
                       , _formConfig_patchReq         = patchChannel usingCookie
                       , _formConfig_setPrimaryKey    = setChannelPk
                       , _formConfig_header           = mkHeader
                       , _formConfig_routeAfterDelete = (`channelsLink` mempty)
                       , _formConfig_editRoute        = channelLink
                       }
  setChannelPk (Just (ChannelId pk)) x = x { _channelId = pure pk }
  setChannelPk Nothing               x = x { _channelId = pure 0 }

  mkHeader x
    | fromMaybe 0 (getLast (_channelId x)) > 0 = "Channel - "
    <> fromMaybe "?" (getLast $ _channelName x)
    | otherwise = "New channel"

channelIdProp :: Property ChannelT SerialInt64
channelIdProp = prop "Id" channelId (Proxy :: Proxy "_channelId")

channelNameProp :: Property ChannelT Text
channelNameProp = prop "Name" channelName (Proxy :: Proxy "_channelName")

