{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.Map                      as Map
import           Data.Proxy
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Time
import           Reflex
import           Reflex.Dom              hiding ( Link(..)
                                                , fileInput
                                                , rangeInput
                                                , textInput
                                                )
import           Servant.API             hiding ( URI(..) )
import           Servant.Links           hiding ( URI(..) )
import           Servant.Router
import           URI.ByteString

import           Components

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
  -> m (Event t Link)
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

inputHandler :: WidgetConstraint js t m => RouteT InputApi m (Event t URI)
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

printMaterial :: DomBuilder t m => Material -> m ()
printMaterial M14404 = text "1.4404"
printMaterial M14307 = text "1.4307"

basicHandler
  :: (MonadIO m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
basicHandler = do
  el "h1" $ text "Basic"

  el "form" $ do
    _  <- labeled "Hello world" textInput (inputConfig "Hello world")

    n1 <- labeled
      "Numeric input"
      numberInput
      (inputConfig' (def { _numberRange_precision = Just 3 }) (0 :: Double))
    _ <- labeled
      "Overridable input"
      (overridableNumberInput
        (fmapMaybe Prelude.id $ updated (_inputEl_value n1))
      )
      (inputConfig $ Overridable (0 :: Double) Nothing)

    _ <- labeled "Color"
                 textInput
                 def { _inputConfig_attributes = "type" =: "color" }

    _ <- labeled "Email"
                 textInput
                 def { _inputConfig_attributes = "type" =: "email" }
    _ <- labeled "Image"
                 textInput
                 def { _inputConfig_attributes = "type" =: "image" }
    _ <- labeled "Search"
                 textInput
                 def { _inputConfig_attributes = "type" =: "search" }
    _ <- labeled
      "Submit"
      textInput
      (inputConfig "Submit") { _inputConfig_attributes = "type" =: "submit" }
    _ <- labeled "Tel"
                 textInput
                 def { _inputConfig_attributes = "type" =: "tel" }
    _ <- labeled "Url"
                 textInput
                 def { _inputConfig_attributes = "type" =: "url" }
    pure ()
  pure never


checkboxHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
checkboxHandler = do
  el "h1" $ text "Checkbox"

  el "form" $ do
    _ <- checkboxInput "I agree to the terms" (inputConfig False)

    _ <- labeled
      "Material"
      checkboxesInput
      materialExample { _inputConfig_status = constDyn $ InputError "Error" }

    _ <- labeled
      "Disabled"
      checkboxesInput
      materialExample { _inputConfig_status = constDyn InputDisabled }

    _ <- labeled
      "Success"
      checkboxesInput
      materialExample
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }
    pure ()

  elClass "form" "vertical" $ do
    _ <- labeled "Terms and conditions"
                 (checkboxesInputMap (Map.singleton () (text "I agree")))
                 (inputConfig Set.empty)

    _ <- labeled "Vertical layout" checkboxesInput materialExample

    pure ()

  el "h2" $ text "Toggle"
  el "form" $ do
    _ <- toggleInput "Turn it on" (inputConfig False)
    _ <- labeled "Material" togglesInput materialExample
    _ <- labeled
      "Material"
      togglesInput
      materialExample { _inputConfig_status = constDyn $ InputError "Error" }
    _ <- labeled
      "Disabled"
      togglesInput
      materialExample { _inputConfig_status = constDyn InputDisabled }
    _ <- labeled
      "Success"
      togglesInput
      materialExample
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }
    pure ()

  pure never
  where materialExample = materialExample' Set.singleton

materialExample'
  :: (DomBuilder t m)
  => (Material -> f Material)
  -> EnumInputConfig t m (f Material)
materialExample' bump = inputConfig' (OpElem printMaterial) (bump M14307)

comboboxHandler
  :: (PostBuild t m, DomBuilder t m, MonadIO m, MonadHold t m, MonadFix m)
  => m (Event t URI)
comboboxHandler = do
  el "h1" $ text "Combobox"
  el "form" $ do
    x <- labeled "Flavours" (comboboxInput showOpt flavors) def

    display (_inputEl_value x)

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
    _ <- labeled "Ice Cream Flavor" (datalistInput flavors) def
    _ <- labeled
      "Ice Cream Flavor"
      (datalistInput flavors)
      def
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Pick your favorite ice cream flavor")
        }
    _ <- labeled "Disabled"
                 (datalistInput flavors)
                 def { _inputConfig_status = constDyn InputDisabled }
    _ <- labeled
      "Error"
      (datalistInput flavors)
      def
        { _inputConfig_status = constDyn $ InputError "Unknown ice cream flavor"
        }
    _ <- labeled
      "Success"
      (datalistInput flavors)
      def { _inputConfig_status = constDyn $ InputSuccess "Changes saved" }
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
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     )
  => m (Event t URI)
fileHandler = do
  el "h1" $ text "File"

  el "form" $ do
    _ <- labeled "Attachment" fileInput def
    _ <- labeled
      "Attachment"
      fileInput
      def
        { _inputConfig_status = constDyn
                                  $ InputNeutral (Just "Max file size: 128 MB")
        }
    _ <- labeled "Disabled"
                 fileInput
                 def { _inputConfig_status = constDyn InputDisabled }
    _ <- labeled
      "Error"
      fileInput
      def { _inputConfig_status = constDyn $ InputError "Exceed file size limit"
          }
    _ <- labeled
      "Success"
      fileInput
      def { _inputConfig_status = constDyn $ InputSuccess "File accepted" }

    _ <- labeled
      "Dropzone"
      fileDropzone
      def { _inputConfig_status = constDyn $ InputSuccess "File accepted" }

    pure ()

  pure never

groupHandler
  :: (MonadIO m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
groupHandler = do
  el "h1" $ text "Input Group"

  el "form" $ do
    labeled "Domain" (`inputGroup` content) def
    labeled
      "Domain"
      (`inputGroup` content)
      def
        { _inputConfig_status = constDyn
                                  $ InputNeutral (Just "Choose a domain name")
        }
    labeled "Disabled"
            (`inputGroup` content)
            def { _inputConfig_status = constDyn InputDisabled }
    labeled
      "Error"
      (`inputGroup` content)
      def { _inputConfig_status = constDyn $ InputError "Unreachable domain" }
    labeled
      "Success"
      (`inputGroup` content)
      def { _inputConfig_status = constDyn $ InputSuccess "Domain ready" }

  pure never

 where
  content = do
    _ <- labeled "Protocol"
                 selectInput
                 (inputConfig' (OpElem printProtocol) (Just Http))
    _ <- labeled "Domain" textInput (inputConfig "")
    _ <- labeled "Toplevel"
                 selectInput
                 (inputConfig' (OpElem printDomain) (Just Dev))
    pure ()


data Protocol = Http | Https deriving (Eq, Show, Enum, Bounded)

instance Default Protocol where
  def = Http

printProtocol :: DomBuilder t m => Protocol -> m ()
printProtocol Http  = text "http://"
printProtocol Https = text "https://"

data ToplevelDomain = Dev | Com | Org deriving (Eq, Show, Enum, Bounded)

instance Default ToplevelDomain where
  def = Dev

printDomain :: DomBuilder t m => ToplevelDomain -> m ()
printDomain Dev = text ".dev"
printDomain Com = text ".com"
printDomain Org = text ".org"

markdownHandler :: WidgetConstraint js t m => m (Event t URI)
markdownHandler = do
  _ <- markdownInput
    (inputConfig'
      cdnAceConfig
      "# Markdown editor\n\n```haskell\nmain :: IO ()\nmain = pure ()\n```\n\nWith LaTeX: `$x^2$`"
    )
  signpost def markdownCheatSheet
  pure never

passwordHandler
  :: (MonadIO m, MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
passwordHandler = do
  el "h1" $ text "Password"

  el "form" $ do
    _ <- labeled "Password" passwordInput def
    _ <- labeled
      "Password"
      passwordInput
      def
        { _inputConfig_status = constDyn $ InputNeutral
          (Just
            "Use 8 or more characters with a mix of letters, numbers & symbols"
          )
        }
    _ <- labeled "Disabled"
                 passwordInput
                 def { _inputConfig_status = constDyn InputDisabled }
    _ <- labeled
      "Error"
      passwordInput
      def
        { _inputConfig_status = constDyn
          $ InputError "Use 8 or more characters for your password"
        }
    _ <- labeled
      "Success"
      passwordInput
      def
        { _inputConfig_status = constDyn
                                  $ InputSuccess "Password meets requirements"
        }
    pure ()

  pure never

radioHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
radioHandler = do
  el "h1" $ text "Radio"

  el "form" $ do
    r1 <- labeled
      "Select material"
      radioInput
      materialExample
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Select the material to be used")
        }
    r2 <- labeled
      "Error"
      radioInput
      materialExample { _inputConfig_status = constDyn $ InputError "Error" }

    r3 <- labeled
      "Disabled"
      radioInput
      materialExample { _inputConfig_status = constDyn InputDisabled }

    r4 <- labeled
      "Success"
      radioInput
      materialExample
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }
    display (_inputEl_value r1)
    text ", "
    display (_inputEl_value r2)
    text ", "
    display (_inputEl_value r3)
    text ", "
    display (_inputEl_value r4)
    pure ()

  el "p" $ do
    text "The radio component is currently broken due to "
    elAttr "a"
           ("href" =: "https://github.com/reflex-frp/reflex-dom/issues/412")
           (text "https://github.com/reflex-frp/reflex-dom/issues/412")
    text "."

  pure never
  where materialExample = materialExample' Just

rangeHandler
  :: (MonadIO m, PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => m (Event t URI)
rangeHandler = do
  el "h1" $ text "Range"

  el "form" $ do
    _ <- labeled "Volume" rangeInput (inputConfig (50 :: Double))
    _ <- labeled
      "Error"
      rangeInput
      (inputConfig (50 :: Double))
        { _inputConfig_status = constDyn $ InputError "System error"
        }

    _ <- labeled
      "Disabled"
      rangeInput
      (inputConfig (50 :: Double)) { _inputConfig_status = constDyn
                                     InputDisabled
                                   }

    _ <- labeled
      "Success"
      rangeInput
      (inputConfig (50 :: Double))
        { _inputConfig_status = constDyn $ InputSuccess "Changes saved"
        }
    pure ()

  pure never

selectHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
selectHandler = do
  el "h1" $ text "Select"

  el "form" $ do
    _ <- labeled
      "Select material"
      selectInput
      materialExample
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Select the material to be used")
        }
    _ <- labeled
      "Error"
      selectInput
      materialExample { _inputConfig_status = constDyn $ InputError "Error" }

    _ <- labeled
      "Disabled"
      selectInput
      materialExample { _inputConfig_status = constDyn InputDisabled }

    _ <- labeled
      "Success"
      selectInput
      materialExample
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }
    pure ()

  pure never
  where materialExample = materialExample' Just

textareaHandler :: (MonadIO m, PostBuild t m, DomBuilder t m) => m (Event t URI)
textareaHandler = do
  el "h1" $ text "Textarea"

  el "form" $ do
    _ <- labeled
      "Select material"
      textAreaInput
      def
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Select the material to be used")
        }
    _ <- labeled "Error"
                 textAreaInput
                 def { _inputConfig_status = constDyn $ InputError "Error" }

    _ <- labeled "Disabled"
                 textAreaInput
                 def { _inputConfig_status = constDyn InputDisabled }

    _ <- labeled
      "Success"
      textAreaInput
      def { _inputConfig_status = constDyn $ InputSuccess "Success message" }
    pure ()

  pure never

timeHandler
  :: (MonadIO m, PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => m (Event t URI)
timeHandler = do
  el "h1" $ text "Time"

  el "form" $ do
    t1 <- labeled "Time"
                  timeOfDayInput
                  (inputConfig (Just (TimeOfDay 11 12 14)))
    _ <- labeled "Error"
                 timeOfDayInput
                 def { _inputConfig_status = constDyn $ InputError "Error" }

    _ <- labeled "Disabled"
                 timeOfDayInput
                 def { _inputConfig_status = constDyn InputDisabled }

    _ <- labeled
      "Success"
      timeOfDayInput
      def { _inputConfig_status = constDyn $ InputSuccess "Success message" }
    display (_inputEl_value t1)
    pure ()

  el "h2" $ text "Other date inputs"
  el "form" $ do
    d1 <- labeled "Day"
                  (requiredInput dateInput)
                  (inputConfig (Just (fromGregorian 2021 03 14)))
    dt1 <- labeled
      "Local time"
      localtimeInput
      (inputConfig
        (Just (LocalTime (fromGregorian 2021 03 14) (TimeOfDay 15 30 00)))
      )
    w1 <- labeled "Week" weekInput (inputConfig (Just (2021, 30)))
    m1 <- labeled "Month" monthInput (inputConfig (Just (2021, 03)))
    display (_inputEl_value d1)
    text ", "
    display (_inputEl_value dt1)
    text ", "
    display (_inputEl_value w1)
    text ", "
    display (_inputEl_value m1)
    pure ()

  pure never
