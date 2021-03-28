{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend.Core
  ( coreHandler
  , coreLinks
  , CoreApi
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Proxy
import           Data.Text                      ( pack )
import           URI.ByteString
import           Reflex
import           Reflex.Dom              hiding ( rangeInput
                                                , Link(..)
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router

import           Components

-- brittany-disable-next-binding
type CoreApi = "core" :> "button" :> View
        :<|> "core" :> "alert" :> View
        :<|> "core" :> "progress" :> View
        :<|> "core" :> "tag" :> View

coreApi :: Proxy CoreApi
coreApi = Proxy

coreButtonLink, coreAlertLink, coreProgressLink, coreTagLink :: Link
coreButtonLink :<|> coreAlertLink :<|> coreProgressLink :<|> coreTagLink =
  allLinks coreApi

coreLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t ())
coreLinks dynUri = safelinkGroup
  (text "Core components")
  [ safelink dynUri coreAlertLink $ text "Alert"
  , safelink dynUri coreButtonLink $ text "Button"
  , safelink dynUri coreProgressLink $ text "Progress"
  , safelink dynUri coreTagLink $ text "Tag"
  ]


coreHandler :: MonadWidget t m => RouteT CoreApi m (Event t URI)
coreHandler = coreButton :<|> coreAlert :<|> coreProgress :<|> coreTag

coreAlert :: (DomBuilder t m, PostBuild t m) => m (Event t URI)
coreAlert = do
  el "h1" $ text "Alert"
  el "h2" $ text "App level alerts"
  el "p"
    $ text
        "App level alerts should be placed above the header. They are for global error and warning messages."
  _ <-
    alertAppLevel def { _alertConfig_status = Danger }
                  "Your license is about to expire."
      $ do
          _ <- btn def (text "Renew")
          elAttr "a" ("href" =: "#") (text "Click here")
  _ <- alertAppLevel
    def { _alertConfig_status = Warning }
    "This feature is under development. For more information, visit the documentation or contact Ferp-hs support."
    (pure ())
  _ <- alertAppLevel def { _alertConfig_status = Info }
                     "You can customize your host in the settings panel."
                     (pure ())

  el "h2" $ text "Standard alerts"
  el "p"
    $ text
        "These standard alerts can be used within the content area or within components, such as a card or modal."
  _ <-
    alert def { _alertConfig_status = Danger }
          "Your license is about to expire."
      $ do
          _ <- btn def (text "Renew")
          elAttr "a" ("href" =: "#") (text "Click here")
  _ <- alert def { _alertConfig_status = Danger }
             "The host CPU is running low."
             (pure ())
  _ <- alert
    def { _alertConfig_status = Warning }
    "This feature is under development. For more information, visit the documentation or contact Ferp-hs support."
    (pure ())
  _ <- alert def { _alertConfig_status = Info }
             "You can customize your host in the settings panel."
             (pure ())
  _ <- alert def { _alertConfig_status = Success }
             "Your container has been created."
             (pure ())

  el "h2" $ text "Compact alerts"
  el "p" $ text "If space is limited, the compact variant can be used."
  _ <- alert
    def { _alertConfig_status = Success, _alertConfig_size = CompactSize }
    "Your container has been created."
    (pure ())

  pure never

coreProgress
  :: (MonadHold t m, DomBuilder t m, PostBuild t m) => m (Event t URI)
coreProgress = do
  el "h1" $ text "Progress"
  el "h2" $ text "Progress bars"
  el "p"
    $ text
        "Progress bars are to indicate 'This wil take a while', where 'a while' is more than 10 seconds."
  progressBar mempty (constDyn (Just 0.5))
  el "p"
    $ text "When there is no estimate, an indeterminate progress can be shown"
  progressBar mempty (constDyn Nothing)
  el "p" $ text "Optionally a label can be placed on the right"
  progressBarLabel mempty (constDyn (Just 0.5))

  el "h2" $ text "Spinners"
  el "p"
    $ text "For 1-10 seconds or when space is limited, a spinner can be used."
  spinner Inline "Loading ..."
  spinner Large  "Loading ..."
  spinner Medium "Loading ..."
  spinner Small  "Loading ..."

  pure never

coreTag
  :: (MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
  => m (Event t URI)
coreTag = do
  el "h1" $ text "Tag"
  el "p" $ text "Labels show concise metadata in a compact format."

  mapM_ (tagEl def { _tagConfig_color = TagPurple })
        ["Fruit", "Meat", "Drink", "Vegetable"]

  el "p" $ text "They can have different colors"
  mapM_
    (\(c, lbl) -> tagEl def { _tagConfig_color = c } lbl)
    [ (TagGrey               , "Grey (default)")
    , (TagPurple             , "Purple")
    , (TagOrange             , "Orange")
    , (TagLightGreen         , "Frost 1")
    , (TagCyan               , "Frost 2")
    , (TagLightBlue          , "Frost 3")
    , (TagStatusColor Success, "Success")
    , (TagStatusColor Info   , "Info")
    , (TagStatusColor Warning, "Warning")
    , (TagStatusColor Danger , "Danger")
    ]

  el "h2" $ text "Badges"
  el "p" $ text "Badges show a numerical value within another element"
  mapM_
    (uncurry badge')
    [ (TagGrey               , 0)
    , (TagPurple             , 1)
    , (TagOrange             , 2)
    , (TagLightGreen         , 3)
    , (TagCyan               , 4)
    , (TagLightBlue          , 5)
    , (TagStatusColor Success, 1)
    , (TagStatusColor Info   , 22)
    , (TagStatusColor Warning, 64)
    , (TagStatusColor Danger , 102)
    ]

  el "h2" $ text "Labels with badges"
  el "p" $ text "Labels may contain badges. "
  _ <- tagEl def { _tagConfig_badge = Just 100 } "Development"
  _ <- tagEl def { _tagConfig_badge = Just 12 } "Test"
  _ <- tagEl def { _tagConfig_badge = Just 1 } "Acceptance"
  _ <- tagEl def { _tagConfig_badge = Just 0 } "Production"

  el "h2" $ text "Clickable tags"
  el "p" $ text "Labels may be clickable or dismissable."
  clickEv <- tagEl def { _tagConfig_action = Just TagClick } "Clickable"
  dismissEv <- tagEl def { _tagConfig_action = Just TagDismiss } "Dismissable"

  (countDyn :: Dynamic t Integer) <- count $ leftmost [clickEv, dismissEv]
  el "p" $ dynText $ fmap (("Counter: " <>) . pack . show) countDyn

  pure never


coreButton
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
coreButton = do
  el "h1" $ text "Button"
  el "p"
    $ text
        "Primary buttons are used for the primary action. \
       \  Secondary buttons are used for actions which complement the primary action or to reduce visual noise. \
       \  Tertiary buttons are used in special occasions, and can also be used inline."
  btn1Ev <- btn def (text "Primary button")
  btn2Ev <- btn def { _buttonConfig_priority = ButtonSecondary }
                (text "Secondary button")
  btn3Ev <- btn def { _buttonConfig_priority = ButtonTertiary }
                (text "Tertiary button")

  el "p"
    $ text
        "The primary buttons also come in the success, warning and danger variant."
  btn1SEv <- btn def { _buttonConfig_priority = ButtonPrimary Success }
                 (text "Success")
  btn1WEv <- btn def { _buttonConfig_priority = ButtonPrimary Warning }
                 (text "Warning")
  btn1DEv <- btn def { _buttonConfig_priority = ButtonPrimary Danger }
                 (text "Danger")

  el "p" $ text "Icons and badges can be used"
  btnIcon2 <- btn def { _buttonConfig_priority = ButtonSecondary }
                  (icon def timesIcon >> spantext "Cancel")
  btnIcon1     <- btn def (icon def successStandardIcon >> spantext "Ok")

  btnIconOnly3 <- btn def { _buttonConfig_priority = ButtonTertiary }
                      (icon def timesIcon)
  btnIconOnly2 <- btn def { _buttonConfig_priority = ButtonSecondary }
                      (icon def timesIcon)
  btnIconOnly1 <- btn def (icon def successStandardIcon)

  btnBadge2    <- btn def { _buttonConfig_priority = ButtonSecondary }
                      (spantext "Archive items" >> badge' def 10)
  btnBadge1 <- btn def (spantext "Mark as read" >> badge' def 100)

  el "h2" $ text "Dropdown"
  dropdownEv <- btnDropdown def (text "Dropdown") $ do
    dropdownHeader "Dropdown header"
    actionEv <- btn def (text "Action")
    _        <- btn def { _buttonConfig_state = constDyn ActionDisabled }
                    (text "Disabled link")
    divider
    loremEv      <- btn def (text "Lorem.")
    loremIpsumEv <- btnDropdown def (text "Lorem ipsum.") $ do
      fooEv  <- btn def (text "Foo.")
      barEv  <- btnDropdown def (text "Bar.") $ btn def (text "Baz.")
      foo2Ev <- btn def (text "Foo 2.")
      pure $ leftmost [fooEv, barEv, foo2Ev]
    ipsumEv <- btn def (text "Ipsum")
    pure $ leftmost [actionEv, loremEv, loremIpsumEv, ipsumEv]

  el "h2" $ text "Interaction"
  btnState1 <- btn
    ButtonConfig { _buttonConfig_priority = def
                 , _buttonConfig_state    = constDyn ActionSuccess
                 , _buttonConfig_class    = ""
                 }
    (icon def timesIcon >> spantext "Save")
  btnState2 <- btn
    ButtonConfig { _buttonConfig_priority = ButtonSecondary
                 , _buttonConfig_state    = constDyn ActionError
                 , _buttonConfig_class    = ""
                 }
    (icon def timesIcon >> spantext "Save")
  btnState3 <- btn
    ButtonConfig { _buttonConfig_priority = ButtonSecondary
                 , _buttonConfig_state    = constDyn ActionDisabled
                 , _buttonConfig_class    = ""
                 }
    (icon def timesIcon >> spantext "Save" >> badge' def 10)

  el "h2" $ text "Button Group"
  _ <- btnGroup $ do
    mapM_
      (\(c, lbl) -> btn def { _buttonConfig_priority = c } (text lbl))
      [ (ButtonPrimary Info, "Add")
      , (ButtonPrimary Info, "Edit")
      , (ButtonPrimary Info, "Delete")
      ]
    btnOverflow (def { _buttonConfig_priority = ButtonPrimary Info }) $ do
      downloadEv <- btn def (text "Download")
      deleteEv   <- btn def (text "Delete")
      pure $ leftmost [downloadEv, deleteEv]

  btnGroup $ mapM_
    (\(c, lbl) -> btn def { _buttonConfig_priority = c } (text lbl))
    [ (ButtonSecondary, "Add")
    , (ButtonSecondary, "Edit")
    , (ButtonSecondary, "Delete")
    ]
  btnGroup $ mapM_
    (\(c, lbl) -> btn def { _buttonConfig_priority = c } (text lbl))
    [ (ButtonTertiary, "Add")
    , (ButtonTertiary, "Edit")
    , (ButtonTertiary, "Delete")
    ]
  btnGroup $ mapM_
    (\(c, lbl) -> btn def { _buttonConfig_priority = c } (text lbl))
    [ (ButtonPrimary Success, "Success")
    , (ButtonPrimary Warning, "Warning")
    , (ButtonPrimary Danger , "Danger")
    ]

  (countDyn :: Dynamic t Integer) <- count $ leftmost
    [ btn1Ev
    , btn2Ev
    , btn3Ev
    , btn1SEv
    , btn1WEv
    , btn1DEv
    , btnIcon2
    , btnIcon1
    , btnBadge2
    , btnBadge1
    , btnIconOnly1
    , btnIconOnly2
    , btnIconOnly3
    , btnState1
    , btnState2
    , btnState3
    , dropdownEv
    ]
  el "p" $ dynText $ fmap (("Counter: " <>) . pack . show) countDyn

  pure never

