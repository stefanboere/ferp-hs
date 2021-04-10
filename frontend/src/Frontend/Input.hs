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
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Time
import           Language.Javascript.JSaddle.Types
                                                ( MonadJSM )
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
import           Reflex.Markdown

-- brittany-disable-next-binding
type InputApi = "input" :> "basic" :> View
           :<|> "input" :> "checkbox" :> View
           :<|> "input" :> "combobox" :> View
           :<|> "input" :> "datalist" :> View
           :<|> "input" :> "file" :> View
           :<|> "input" :> "group" :> View
           :<|> "input" :> "markdown" :> View
           :<|> "input" :> "password" :> View
           :<|> "input" :> "radio" :> View
           :<|> "input" :> "range" :> View
           :<|> "input" :> "select" :> View
           :<|> "input" :> "textarea" :> View
           :<|> "input" :> "time" :> View

inputApi :: Proxy InputApi
inputApi = Proxy

inputBasicLink, inputCheckboxLink, inputComboBoxLink, inputDatalist, inputFileLink, inputGroupLink, inputMarkdownLink, inputPasswordLink, inputRadioLink, inputRangeLink, inputSelectLink, inputTextareaLink, inputTimeLink
  :: Link
inputBasicLink :<|> inputCheckboxLink :<|> inputComboBoxLink :<|> inputDatalist :<|> inputFileLink :<|> inputGroupLink :<|> inputMarkdownLink :<|> inputPasswordLink :<|> inputRadioLink :<|> inputRangeLink :<|> inputSelectLink :<|> inputTextareaLink :<|> inputTimeLink
  = allLinks inputApi

inputLinks
  :: (MonadFix m, MonadIO m, DomBuilder t m, PostBuild t m)
  => Dynamic t URI
  -> m (Event t ())
inputLinks dynUri = safelinkGroup
  (text "Input elements")
  [ safelink dynUri inputBasicLink $ text "Basic"
  , safelink dynUri inputCheckboxLink $ text "Checkbox"
  , safelink dynUri inputComboBoxLink $ text "Combobox"
  , safelink dynUri inputDatalist $ text "Datalist"
  , safelink dynUri inputFileLink $ text "File"
  , safelink dynUri inputGroupLink $ text "Input Group"
  , safelink dynUri inputMarkdownLink $ text "Markdown"
  , safelink dynUri inputPasswordLink $ text "Password"
  , safelink dynUri inputRadioLink $ text "Radio"
  , safelink dynUri inputRangeLink $ text "Range"
  , safelink dynUri inputSelectLink $ text "Select"
  , safelink dynUri inputTextareaLink $ text "Textarea"
  , safelink dynUri inputTimeLink $ text "Time"
  ]

inputHandler :: MonadWidget t m => RouteT InputApi m (Event t URI)
inputHandler =
  basicHandler
    :<|> checkboxHandler
    :<|> comboboxHandler
    :<|> datalistHandler
    :<|> fileHandler
    :<|> groupHandler
    :<|> markdownHandler
    :<|> passwordHandler
    :<|> radioHandler
    :<|> rangeHandler
    :<|> selectHandler
    :<|> textareaHandler
    :<|> timeHandler

instance Default Text where
  def = mempty

data Material = M14404 | M14307 deriving (Eq, Show, Ord, Enum, Bounded)

instance Default Material where
  def = M14404

instance HasLabel Material where
  toLabel M14404 = "1.4404"
  toLabel M14307 = "1.4307"

basicHandler
  :: (MonadIO m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
basicHandler = do
  el "h1" $ text "Basic"

  el "form" $ do
    _ <- textInput def { _inputConfig_label        = constDyn "Text input"
                       , _inputConfig_initialValue = "Hello world"
                       }
    n1 <- numberInput
      def { _numberInputConfig_precision = Just 3 }
      def { _inputConfig_label        = constDyn "Numeric input"
          , _inputConfig_initialValue = 0 :: Double
          }
    _ <- overridableNumberInput
      (fmapMaybe Prelude.id $ updated n1)
      def { _inputConfig_label        = constDyn "Overridable input"
          , _inputConfig_initialValue = Overridable (0 :: Double) Nothing
          }
    _ <- textInput def { _inputConfig_label      = constDyn "Color"
                       , _inputConfig_attributes = "type" =: "color"
                       }
    _ <- textInput def { _inputConfig_label      = constDyn "Email"
                       , _inputConfig_attributes = "type" =: "email"
                       }
    _ <- textInput def { _inputConfig_label      = constDyn "Image"
                       , _inputConfig_attributes = "type" =: "image"
                       }
    _ <- textInput def { _inputConfig_label      = constDyn "Search"
                       , _inputConfig_attributes = "type" =: "search"
                       }
    _ <- textInput (inputConfig "Submit") { _inputConfig_label      = constDyn
                                            "Submit"
                                          , _inputConfig_attributes = "type"
                                            =: "submit"
                                          }
    _ <- textInput def { _inputConfig_label      = constDyn "Tel"
                       , _inputConfig_attributes = "type" =: "tel"
                       }
    _ <- textInput def { _inputConfig_label      = constDyn "Url"
                       , _inputConfig_attributes = "type" =: "url"
                       }
    pure ()
  pure never


checkboxHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
checkboxHandler = do
  el "h1" $ text "Checkbox"

  el "form" $ do
    _ <- checkboxInput "I agree to the terms" (inputConfig False)

    _ <- checkboxesInput (inputConfig (Set.singleton M14307))
      { _inputConfig_label  = constDyn "Material"
      , _inputConfig_status = constDyn $ InputError "Error"
      }

    _ <- checkboxesInput (inputConfig (Set.singleton M14307))
      { _inputConfig_label  = constDyn "Disabled"
      , _inputConfig_status = constDyn InputDisabled
      }

    _ <- checkboxesInput (inputConfig (Set.singleton M14307))
      { _inputConfig_label  = constDyn "Success"
      , _inputConfig_status = constDyn $ InputSuccess "Success message"
      }
    pure ()

  elClass "form" "vertical" $ do
    _ <- checkboxesInputMap
      (Map.singleton () "I agree")
      (inputConfig Set.empty) { _inputConfig_label = constDyn
                                "Terms and conditions"
                              }

    _ <- checkboxesInput (inputConfig (Set.singleton M14307))
      { _inputConfig_label = constDyn "Vertical layout"
      }
    pure ()

  el "h2" $ text "Toggle"
  el "form" $ do
    _ <- toggleInput "Turn it on" (inputConfig False)
    _ <- togglesInput (inputConfig (Set.singleton M14307))
      { _inputConfig_label = constDyn "Material"
      }
    _ <- togglesInput (inputConfig (Set.singleton M14307))
      { _inputConfig_label  = constDyn "Material"
      , _inputConfig_status = constDyn $ InputError "Error"
      }
    _ <- togglesInput (inputConfig (Set.singleton M14307))
      { _inputConfig_label  = constDyn "Disabled"
      , _inputConfig_status = constDyn InputDisabled
      }
    _ <- togglesInput (inputConfig (Set.singleton M14307))
      { _inputConfig_label  = constDyn "Success"
      , _inputConfig_status = constDyn $ InputSuccess "Success message"
      }
    pure ()

  pure never

comboboxHandler
  :: (PostBuild t m, DomBuilder t m, MonadIO m, MonadHold t m, MonadFix m)
  => m (Event t URI)
comboboxHandler = do
  el "h1" $ text "Combobox"
  el "form" $ do
    x <- comboboxInput showOpt
                       flavors
                       def { _inputConfig_label = constDyn "Flavours" }

    display x

  pure never
 where
  showOpt k v = dynText ((<> " ") . pack . show <$> k) >> dynText v
  flavors = constDyn $ Map.fromList $ zip
    [(10 :: Integer), 9 ..]
    ["Cherry", "Mint chip", "Vanilla", "Lemon"]

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

fileHandler
  :: ( MonadIO m
     , PostBuild t m
     , DomBuilder t m
     , MonadJSM m
     , MonadFix m
     , MonadHold t m
     )
  => m (Event t URI)
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

    _ <- fileDropzone def
      { _inputConfig_label  = constDyn "Dropzone"
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

markdownHandler :: MonadWidget t m => m (Event t URI)
markdownHandler = do
  headD <- codeInputScripts
  _     <- whenLoaded [headD] blank $ markdownInput
    "# Markdown editor\n\n```haskell\nmain :: IO ()\nmain = pure ()\n```\n\nWith LaTeX: `$x^2$`"
    never
  pure never

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

textareaHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
textareaHandler = do
  el "h1" $ text "Textarea"

  el "form" $ do
    _ <- textAreaInput def
      { _inputConfig_label  = constDyn "Select material"
      , _inputConfig_status = constDyn
        $ InputNeutral (Just "Select the material to be used")
      }
    _ <- textAreaInput def { _inputConfig_label  = constDyn "Error"
                           , _inputConfig_status = constDyn $ InputError "Error"
                           }

    _ <- textAreaInput def { _inputConfig_label  = constDyn "Disabled"
                           , _inputConfig_status = constDyn InputDisabled
                           }

    _ <- textAreaInput def
      { _inputConfig_label  = constDyn "Success"
      , _inputConfig_status = constDyn $ InputSuccess "Success message"
      }
    pure ()

  pure never

timeHandler
  :: (MonadIO m, PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => m (Event t URI)
timeHandler = do
  el "h1" $ text "Time"

  el "form" $ do
    t1 <- timeInput
      def
      (inputConfig (Just (TimeOfDay 11 12 14))) { _inputConfig_label = constDyn
                                                  "Time"
                                                }
    _ <- timeInput
      def
      def { _inputConfig_label  = constDyn "Error"
          , _inputConfig_status = constDyn $ InputError "Error"
          }

    _ <- timeInput
      def
      def { _inputConfig_label  = constDyn "Disabled"
          , _inputConfig_status = constDyn InputDisabled
          }

    _ <- timeInput
      def
      def { _inputConfig_label  = constDyn "Success"
          , _inputConfig_status = constDyn $ InputSuccess "Success message"
          }
    display t1
    pure ()

  el "h2" $ text "Other date inputs"
  el "form" $ do
    d1 <- dateInput
      def
      (inputConfig (Just (fromGregorian 2021 03 14)))
        { _inputConfig_label = constDyn "Day"
        }
    dt1 <- localtimeInput
      def
      (inputConfig
          (Just (LocalTime (fromGregorian 2021 03 14) (TimeOfDay 15 30 00)))
        )
        { _inputConfig_label = constDyn "Local time"
        }
    w1 <- weekInput
      def
      (inputConfig (Just (2021, 30))) { _inputConfig_label = constDyn "Week" }
    m1 <- monthInput
      def
      (inputConfig (Just (2021, 03))) { _inputConfig_label = constDyn "Month" }
    display d1
    text ", "
    display dt1
    text ", "
    display w1
    text ", "
    display m1
    pure ()

  pure never
