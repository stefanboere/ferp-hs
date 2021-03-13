{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Frontend.Input
  ( inputHandler
  , inputLinks
  , InputApi
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Proxy
import qualified Data.Map                      as Map
import           Data.Text                      ( Text
                                                , pack
                                                )
import           URI.ByteString
import           Reflex
import           Reflex.Dom              hiding ( rangeInput
                                                , fileInput
                                                , textInput
                                                , Link(..)
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router

import           Components

-- brittany-disable-next-binding
type InputApi = "input" :> "basic" :> View
           :<|> "input" :> "checkbox" :> View
           :<|> "input" :> "datalist" :> View
           :<|> "input" :> "file" :> View
           :<|> "input" :> "group" :> View
           :<|> "input" :> "password" :> View
           :<|> "input" :> "radio" :> View
           :<|> "input" :> "range" :> View
           :<|> "input" :> "select" :> View

inputApi :: Proxy InputApi
inputApi = Proxy

inputBasicLink, inputCheckboxLink, inputDatalist, inputFileLink, inputGroupLink, inputPasswordLink, inputRadioLink, inputRangeLink, inputSelectLink
  :: Link
inputBasicLink :<|> inputCheckboxLink :<|> inputDatalist :<|> inputFileLink :<|> inputGroupLink :<|> inputPasswordLink :<|> inputRadioLink :<|> inputRangeLink :<|> inputSelectLink
  = allLinks inputApi

inputLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t ())
inputLinks dynUri = safelinkGroup
  (text "Input elements")
  [ safelink dynUri inputBasicLink $ text "Basic"
  , safelink dynUri inputCheckboxLink $ text "Checkbox"
  , safelink dynUri inputDatalist $ text "Datalist"
  , safelink dynUri inputFileLink $ text "File"
  , safelink dynUri inputGroupLink $ text "Input Group"
  , safelink dynUri inputPasswordLink $ text "Password"
  , safelink dynUri inputRadioLink $ text "Radio"
  , safelink dynUri inputRangeLink $ text "Range"
  , safelink dynUri inputSelectLink $ text "Select"
  ]

inputHandler :: MonadWidget t m => RouteT InputApi m (Event t URI)
inputHandler =
  (formTest >> pure never)
    :<|> checkboxHandler
    :<|> datalistHandler
    :<|> fileHandler
    :<|> groupHandler
    :<|> passwordHandler
    :<|> radioHandler
    :<|> rangeHandler
    :<|> selectHandler

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


checkboxHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
checkboxHandler = do
  el "h1" $ text "Checkbox"

  el "form" $ do
    _ <- checkboxInput (inputConfig False)
      { _inputConfig_label = constDyn "I agree to the terms"
      }

    _ <- checkboxesInput (inputConfig [M14307])
      { _inputConfig_label  = constDyn "Material"
      , _inputConfig_status = constDyn $ InputError "Error"
      }

    _ <- checkboxesInput (inputConfig [M14307])
      { _inputConfig_label  = constDyn "Disabled"
      , _inputConfig_status = constDyn InputDisabled
      }

    _ <- checkboxesInput (inputConfig [M14307])
      { _inputConfig_label  = constDyn "Success"
      , _inputConfig_status = constDyn $ InputSuccess "Success message"
      }
    pure ()

  elClass "form" "vertical" $ do
    _ <- checkboxesInputLbl
      (\() -> "I agree")
      (inputConfig [()]) { _inputConfig_label = constDyn "Terms and conditions"
                         }

    _ <- checkboxesInput (inputConfig [M14307])
      { _inputConfig_label = constDyn "Vertical layout"
      }
    pure ()

  pure never

datalistHandler
  :: (MonadIO m, PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => m (Event t URI)
datalistHandler = do
  el "h1" $ text "Datalist"

  el "form" $ do
    _ <- datalistInput flavors def { _inputConfig_label = "Ice Cream Flavor" }
    _ <- datalistInput
      flavors
      def
        { _inputConfig_label  = constDyn "Ice Cream Flavor"
        , _inputConfig_status = constDyn
          $ InputNeutral (Just "Pick your favorite ice cream flavor")
        }
    _ <- datalistInput
      flavors
      def { _inputConfig_label  = constDyn "Disabled"
          , _inputConfig_status = constDyn InputDisabled
          }
    _ <- datalistInput
      flavors
      def
        { _inputConfig_label  = constDyn "Error"
        , _inputConfig_status = constDyn $ InputError "Unknown ice cream flavor"
        }
    _ <- datalistInput
      flavors
      def { _inputConfig_label  = constDyn "Success"
          , _inputConfig_status = constDyn $ InputSuccess "Changes saved"
          }
    pure ()

  pure never

 where
  flavors = constDyn $ Map.fromList $ zip
    [(1 :: Integer) ..]
    ["Cherry", "Mint chip", "Vanilla", "Lemon"]

fileHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
fileHandler = do
  el "h1" $ text "File"

  el "form" $ do
    _ <- fileInput def { _inputConfig_label = constDyn "Attachment" }
    _ <- fileInput def
      { _inputConfig_label  = constDyn "Attachment"
      , _inputConfig_status = constDyn
                                $ InputNeutral (Just "Max file size: 128 MB")
      }
    _ <- fileInput def { _inputConfig_label  = constDyn "Disabled"
                       , _inputConfig_status = constDyn InputDisabled
                       }
    _ <- fileInput def
      { _inputConfig_label  = constDyn "Error"
      , _inputConfig_status = constDyn $ InputError "Exceed file size limit"
      }
    _ <- fileInput def
      { _inputConfig_label  = constDyn "Success"
      , _inputConfig_status = constDyn $ InputSuccess "File accepted"
      }

    pure ()

  pure never

groupHandler
  :: (MonadIO m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
groupHandler = do
  el "h1" $ text "Input Group"

  el "form" $ do
    inputGroup def { _inputConfig_label = "Domain" } content
    inputGroup
      def
        { _inputConfig_label  = "Domain"
        , _inputConfig_status = constDyn
                                  $ InputNeutral (Just "Choose a domain name")
        }
      content
    inputGroup
      def { _inputConfig_label  = "Disabled"
          , _inputConfig_status = constDyn InputDisabled
          }
      content
    inputGroup
      def { _inputConfig_label  = "Error"
          , _inputConfig_status = constDyn $ InputError "Unreachable domain"
          }
      content
    inputGroup
      def { _inputConfig_label  = "Success"
          , _inputConfig_status = constDyn $ InputSuccess "Domain ready"
          }
      content

  pure never

 where
  content = do
    _ <- selectInput (inputConfig (Just Http)) { _inputConfig_label = constDyn
                                                 "Protocol"
                                               }
    _ <- textInput (inputConfig "") { _inputConfig_label = constDyn "Domain" }
    _ <- selectInput (inputConfig (Just Dev)) { _inputConfig_label = constDyn
                                                "Toplevel"
                                              }
    pure ()


data Protocol = Http | Https deriving (Eq, Show, Enum, Bounded)

instance Default Protocol where
  def = Http

instance HasLabel Protocol where
  toLabel Http  = "http://"
  toLabel Https = "https://"

data ToplevelDomain = Dev | Com | Org deriving (Eq, Show, Enum, Bounded)

instance Default ToplevelDomain where
  def = Dev

instance HasLabel ToplevelDomain where
  toLabel Dev = ".dev"
  toLabel Com = ".com"
  toLabel Org = ".org"

passwordHandler
  :: (MonadIO m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
passwordHandler = do
  el "h1" $ text "Password"

  el "form" $ do
    _ <- passwordInput def { _inputConfig_label = constDyn "Password" }
    _ <- passwordInput def
      { _inputConfig_label  = constDyn "Password"
      , _inputConfig_status = constDyn $ InputNeutral
        (Just
          "Use 8 or more characters with a mix of letters, numbers & symbols"
        )
      }
    _ <- passwordInput def { _inputConfig_label  = constDyn "Disabled"
                           , _inputConfig_status = constDyn InputDisabled
                           }
    _ <- passwordInput def
      { _inputConfig_label  = constDyn "Error"
      , _inputConfig_status = constDyn
        $ InputError "Use 8 or more characters for your password"
      }
    _ <- passwordInput def
      { _inputConfig_label  = constDyn "Success"
      , _inputConfig_status = constDyn
                                $ InputSuccess "Password meets requirements"
      }
    pure ()

  pure never

radioHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
radioHandler = do
  el "h1" $ text "Radio"

  el "form" $ do
    r1 <- radioInput (inputConfig (Just M14307))
      { _inputConfig_label  = constDyn "Select material"
      , _inputConfig_status = constDyn
        $ InputNeutral (Just "Select the material to be used")
      }
    r2 <- radioInput (inputConfig (Just M14307))
      { _inputConfig_label  = constDyn "Error"
      , _inputConfig_status = constDyn $ InputError "Error"
      }

    r3 <- radioInput (inputConfig (Just M14307))
      { _inputConfig_label  = constDyn "Disabled"
      , _inputConfig_status = constDyn InputDisabled
      }

    r4 <- radioInput (inputConfig (Just M14307))
      { _inputConfig_label  = constDyn "Success"
      , _inputConfig_status = constDyn $ InputSuccess "Success message"
      }
    display r1
    text ", "
    display r2
    text ", "
    display r3
    text ", "
    display r4
    pure ()

  el "p" $ do
    text "The radio component is currently broken due to "
    elAttr "a"
           ("href" =: "https://github.com/reflex-frp/reflex-dom/issues/412")
           (text "https://github.com/reflex-frp/reflex-dom/issues/412")
    text "."

  pure never

rangeHandler
  :: (MonadIO m, PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => m (Event t URI)
rangeHandler = do
  el "h1" $ text "Range"

  el "form" $ do
    _ <- rangeInput
      def
      (inputConfig (50 :: Double)) { _inputConfig_label = constDyn "Volume" }
    _ <- rangeInput
      def
      (inputConfig (50 :: Double))
        { _inputConfig_label  = constDyn "Error"
        , _inputConfig_status = constDyn $ InputError "System error"
        }

    _ <- rangeInput
      def
      (inputConfig (50 :: Double)) { _inputConfig_label  = constDyn "Disabled"
                                   , _inputConfig_status = constDyn
                                     InputDisabled
                                   }

    _ <- rangeInput
      def
      (inputConfig (50 :: Double))
        { _inputConfig_label  = constDyn "Success"
        , _inputConfig_status = constDyn $ InputSuccess "Changes saved"
        }
    pure ()

  pure never

selectHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
selectHandler = do
  el "h1" $ text "Select"

  el "form" $ do
    _ <- selectInput (inputConfig (Just M14307))
      { _inputConfig_label  = constDyn "Select material"
      , _inputConfig_status = constDyn
        $ InputNeutral (Just "Select the material to be used")
      }
    _ <- selectInput (inputConfig (Just M14307))
      { _inputConfig_label  = constDyn "Error"
      , _inputConfig_status = constDyn $ InputError "Error"
      }

    _ <- selectInput (inputConfig (Just M14307))
      { _inputConfig_label  = constDyn "Disabled"
      , _inputConfig_status = constDyn InputDisabled
      }

    _ <- selectInput (inputConfig (Just M14307))
      { _inputConfig_label  = constDyn "Success"
      , _inputConfig_status = constDyn $ InputSuccess "Success message"
      }
    pure ()

  pure never

