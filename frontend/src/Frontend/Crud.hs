{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Crud
  ( crudHandler
  , crudLinks
  , CrudApi
  ) where

import           Control.Applicative            ( (<**>) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Functor.Compose
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                , textInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Crud.QueryObject       ( QObj )
import           Servant.Crud.QueryOperator     ( MaybeLast(..) )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router
import           Servant.Subscriber.Reflex
import           URI.ByteString                 ( URI )

import qualified Common.Api                    as Api
import           Common.Auth
import           Common.Schema
import           Components
import           Frontend.Api
import           Frontend.Crud.Datagrid
import           Frontend.Crud.Edit
import           Frontend.Crud.Lookup
import           Frontend.Crud.Utils

-- brittany-disable-next-binding
type CrudApi =
  ("blogs" :>
     (    "all" :> QObj (Api.View Api.Be BlogN) :> View
     :<|> Auth Everyone :> "new" :> View
     :<|> Auth Everyone :> Capture "key" BlogNId :> View
     )
  ) :<|>
  ("channels" :>
     (    "all" :> QObj (Api.View Api.Be ChannelT) :> View
     :<|> Auth Everyone :> "new" :> View
     :<|> Auth Everyone :> Capture "key" ChannelId :> View
     )
  )

crudApi :: Proxy CrudApi
crudApi = Proxy

blogsLink :: Api.View Api.Be BlogN -> Link
newBlogLink :: Link
blogLink :: BlogNId -> Link

channelsLink :: Api.View Api.Be ChannelT -> Link
newChannelLink :: Link
channelLink :: ChannelId -> Link

-- brittany-disable-next-binding
(blogsLink :<|> newBlogLink :<|> blogLink) :<|>
  (channelsLink :<|> newChannelLink :<|> channelLink)
  = allLinks crudApi

crudLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t Link)
crudLinks dynUri = safelinkGroup
  (text "Crud")
  [ safelink dynUri (blogsLink mempty) $ text "Blogs"
  , safelink dynUri (channelsLink mempty) $ text "Channels"
  ]

crudHandler
  :: WidgetConstraint js t m => RouteT CrudApi (ApiWidget t m) (Event t URI)
crudHandler =
  (blogsHandler :<|> blogEdit Nothing :<|> (blogEdit . Just))
    :<|> (channelsHandler :<|> channelEdit Nothing :<|> (channelEdit . Just))

blogsHandler
  :: (WidgetConstraint js t m)
  => Api.View Api.Be BlogN
  -> ApiWidget t m (Event t URI)
blogsHandler = browseForm BrowseFormConfig
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

blogEdit
  :: WidgetConstraint js t m => Maybe BlogNId -> ApiWidget t m (Event t URI)
blogEdit = editForm cfg $ \modBlogEv ->
  el "form"
    $    getCompose
    $    pure mempty
    <**> editWith textEditor blogTitleProp modBlogEv
    <**> editFk blogChannelProp modBlogEv
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
    , _formConfig_routeAfterDelete = blogsLink mempty
    , _formConfig_editRoute        = blogLink
    }
  req = requiredEditor

  coerce (BlogId x) = BlogId x

  setBlogPk (Just (BlogId pk)) x = x { _blogId = pure pk }
  setBlogPk Nothing            x = x { _blogId = pure 0 }

  mkHeader x
    | fromMaybe 0 (unMaybeLast (_blogId x)) > 0 = "Blog - "
    <> fromMaybe "?" (unMaybeLast $ _blogName x)
    | otherwise = "New blog"

blogIdProp :: Property BlogN SerialInt64
blogIdProp = prop "Id" blogId (Proxy :: Proxy "_blogId")

blogChannelProp
  :: (Request (Client m) ~ FreeClient) => FkProperty t m BlogN ChannelT
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
  -> ApiWidget t m (Event t URI)
channelsHandler = browseForm BrowseFormConfig
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
  :: WidgetConstraint js t m => Maybe ChannelId -> ApiWidget t m (Event t URI)
channelEdit = editForm cfg $ \modBlogEv ->
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
                       , _formConfig_routeAfterDelete = channelsLink mempty
                       , _formConfig_editRoute        = channelLink
                       }
  setChannelPk (Just (ChannelId pk)) x = x { _channelId = pure pk }
  setChannelPk Nothing               x = x { _channelId = pure 0 }

  mkHeader x
    | fromMaybe 0 (unMaybeLast (_channelId x)) > 0 = "Channel - "
    <> fromMaybe "?" (unMaybeLast $ _channelName x)
    | otherwise = "New channel"

channelIdProp :: Property ChannelT SerialInt64
channelIdProp = prop "Id" channelId (Proxy :: Proxy "_channelId")

channelNameProp :: Property ChannelT Text
channelNameProp = prop "Name" channelName (Proxy :: Proxy "_channelName")

