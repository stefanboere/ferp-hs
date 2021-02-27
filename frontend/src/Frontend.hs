{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend
  ( main
  )
where

import           Clay                    hiding ( icon
                                                , id
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Either                    ( fromRight )
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Text.Lazy                 ( toStrict )
import           URI.ByteString
import           Reflex
import           Reflex.Dom              hiding ( rangeInput
                                                , Link(..)
                                                )
import           Reflex.Dom.Contrib.Router
                                         hiding ( URI )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router

import           Components

instance Default Text where
  def = mempty

data Material = M14404 | M14307 deriving (Eq, Show, Enum, Bounded)

instance Default Material where
  def = M14404

instance HasLabel Material where
  toLabel M14404 = "1.4404"
  toLabel M14307 = "1.4307"

data Torispherical = Torispherical
  { ts_wall_thickness         :: Maybe Double
  , ts_outside_diameter       :: Maybe Double
  , ts_straight_flange_height :: Maybe Double
  , ts_crown_radius           :: Maybe (Overridable Double)
  , ts_knuckle_radius         :: Maybe (Overridable Double)
  , ts_material               :: Maybe Material
  , ts_memo                   :: Text
  } deriving (Eq, Show)

main :: IO ()
main = mainWidgetWithCss (encodeUtf8 . toStrict $ renderWith compact [] css)
  $ withHeader mainPage

css :: Css
css = do
  appStyle
  inputStyle
  buttonStyle
  accordionStyle
  tableStyle
  alertStyle
  tagStyle

withHeader
  :: (MonadIO m, MonadFix m, PostBuild t m, DomBuilder t m)
  => m (Dynamic t URI)
  -> m ()
withHeader x = do
  rec dynUri <- app cfg (sideNav dynUri) (pure never) actions x
  pure ()

 where
  cfg = HeaderConfig { _headerConfig_appname           = constDyn "Ferp-hs"
                     , _headerConfig_navigationPattern = Sidenav
                     }
  actions = do
    _ <- ahref "#" (constDyn False) $ icon def cogIcon
    pure ()


mainPage :: forall t m . MonadWidget t m => m (Dynamic t URI)
mainPage = do
  let routeHandler = route'
        (\_ uri -> fixFragment uri)
        (\uri -> (fixFragment uri, routeURI myApi handler . fixFragment $ uri))

  rec dynamicRoute <- routeHandler (switch (current changeRoute))
      routeWasSet  <- dyn (snd <$> dynamicRoute) -- Will fire on postbuild
      changeRoute  <- holdDyn never $ fmap (fromRight never) routeWasSet
  return $ fst <$> dynamicRoute

 where
  fixFragment :: URI -> URI
  fixFragment uri@URI { uriFragment = frag, ..} =
    uri { uriPath = fromMaybe "/" frag }

formTest
  :: (MonadIO m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m ()
formTest = el "form" $ do
  wall_thickness <- numberInput
    def { _numberInputConfig_precision = Just 3
        , _numberInputConfig_minValue  = constDyn (Just 0)
        }
    def { _inputConfig_label        = constDyn "Wall thickness"
        , _inputConfig_initialValue = 0 :: Double
        }
  outside_diameter <- numberInput
    def { _numberInputConfig_precision = Just 3 }
    def { _inputConfig_label        = constDyn "Outside diameter"
        , _inputConfig_initialValue = 0 :: Double
        }
  straight_flange_height <- numberInput
    def { _numberInputConfig_precision = Just 3 }
    def { _inputConfig_label        = constDyn "Straight flange height"
        , _inputConfig_initialValue = 0 :: Double
        }
  crown_radius <- overridableNumberInput
    (fmapMaybe Prelude.id $ updated outside_diameter)
    def { _inputConfig_label        = constDyn "Crown radius"
        , _inputConfig_initialValue = Overridable (0 :: Double) Nothing
        }
  knuckle_radius <- overridableNumberInput
    (fmapMaybe (fmap (/ 10)) $ updated outside_diameter)
    def { _inputConfig_label        = constDyn "Knuckle radius"
        , _inputConfig_initialValue = Overridable (0 :: Double) Nothing
        }
  material <- selectInput def { _inputConfig_label        = constDyn "Material"
                              , _inputConfig_initialValue = Nothing
                              }
  memo <- textAreaInput def { _inputConfig_label        = constDyn "Memo"
                            , _inputConfig_initialValue = mempty
                            }
  let dynTori =
        Torispherical
          <$> wall_thickness
          <*> outside_diameter
          <*> straight_flange_height
          <*> crown_radius
          <*> knuckle_radius
          <*> material
          <*> memo
  dynText $ fmap (pack . show) dynTori

-- brittany-disable-next-binding
type MyApi = "input" :> "basic" :> View
        :<|> "input" :> "button" :> View
        :<|> "core" :> "alert" :> View
        :<|> "core" :> "tag" :> View
        :<|> "container" :> "accordion" :> View
        :<|> "container" :> "tab" :> View
        :<|> "container" :> "table" :> View

myApi :: Proxy MyApi
myApi = Proxy

inputBasicLink, inputButtonLink, coreAlertLink, coreTagLink, containerAccordionLink, containerTabLink, containerTableLink
  :: Link
inputBasicLink :<|> inputButtonLink :<|> coreAlertLink :<|> coreTagLink :<|> containerAccordionLink :<|> containerTabLink :<|> containerTableLink
  = allLinks myApi

sideNav
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t ())
sideNav dynUri = leftmost <$> sequence
  [ safelinkGroup
    (text "Core components")
    [ safelink dynUri coreAlertLink $ text "Alert"
    , safelink dynUri coreTagLink $ text "Tag"
    ]
  , safelinkGroup
    (text "Input elements")
    [ safelink dynUri inputBasicLink $ text "Basic"
    , safelink dynUri inputButtonLink $ text "Button"
    ]
  , safelinkGroup
    (text "Containers")
    [ safelink dynUri containerAccordionLink $ text "Accordion"
    , safelink dynUri containerTabLink $ text "Tab"
    , safelink dynUri containerTableLink $ text "Table"
    ]
  ]

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
    (uncurry badge)
    [ (TagGrey               , forceInt <$> 0)
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

 where
  forceInt :: Integer -> Integer
  forceInt = id

containerAccordion
  :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
containerAccordion = do
  el "h1" $ text "Accordion"
  accordion never
            "Header for panel #1"
            (text "This is the content for accordion panel #1")
  accordion never
            "Header for panel #2"
            (text "This is the content for accordion panel #2")
  accordion never
            "Header for panel #3"
            (text "This is the content for accordion panel #3")
  pure never

containerTab
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
containerTab = do
  el "h1" $ text "Tab"
  tabs
    (  1
    =: ("Tab 1", text "Tab 1 content")
    <> (2 :: Int)
    =: ("Tab 2", text "Tab 2 content")
    )
  pure never

containerTable
  :: (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => m (Event t URI)
containerTable = do
  el "h1" $ text "Table"
  _ <- tableDyn
    [ ("First" , \_ r -> dynText (fst <$> r))
    , ("Second", \_ r -> dynText (snd <$> r))
    ]
    (constDyn
      (1 =: ("First row", "Foo bar") <> (2 :: Int) =: ("Second row", "Bazz"))
    )
  pure never

handler :: MonadWidget t m => RouteT MyApi m (Event t URI)
handler =
  inputBasic
    :<|> inputButton
    :<|> coreAlert
    :<|> coreTag
    :<|> containerAccordion
    :<|> containerTab
    :<|> containerTable
 where
  inputBasic  = formTest >> pure never
  inputButton = text "TBD" >> pure never



