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
import           Data.Default
import qualified Data.Map.Strict               as Map
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
  :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m)
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
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
basicHandler = do
  el "h1" $ text "Basic"

  el "form" $ do
    _  <- labeled "Hello world" textInput (inputConfig "b_txt" "Hello world")

    n1 <- labeled
      "Numeric input"
      numberInput
      (inputConfig' (def { _numberRange_precision = Just 3 })
                    "b_num"
                    (0 :: Double)
      )
    _ <- labeled
      "Overridable input"
      (overridableNumberInput
        (fmapMaybe Prelude.id $ updated (_inputEl_value n1))
      )
      (inputConfig "b_overnum" $ Overridable (0 :: Double) Nothing)

    _ <- labeled
      "Color"
      textInput
      (inputConfig "b_color" def) { _inputConfig_attributes = "type" =: "color"
                                  }

    _ <- labeled
      "Email"
      textInput
      (inputConfig "b_email" def) { _inputConfig_attributes = "type" =: "email"
                                  }
    _ <- labeled
      "Image"
      textInput
      (inputConfig "b_img" def) { _inputConfig_attributes = "type" =: "image" }
    _ <- labeled
      "Search"
      textInput
      (inputConfig "b_srch" def) { _inputConfig_attributes = "type" =: "search"
                                 }
    _ <- labeled
      "Submit"
      textInput
      (inputConfig "b_sub" "Submit") { _inputConfig_attributes = "type"
                                       =: "submit"
                                     }
    _ <- labeled
      "Tel"
      textInput
      (inputConfig "b_tel" def) { _inputConfig_attributes = "type" =: "tel" }
    _ <- labeled
      "Url"
      textInput
      (inputConfig "b_url" def) { _inputConfig_attributes = "type" =: "url" }
    pure ()
  pure never


checkboxHandler
  :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m)
  => m (Event t URI)
checkboxHandler = do
  el "h1" $ text "Checkbox"

  el "form" $ do
    _ <- checkboxInput "I agree to the terms" (inputConfig "cbx_n" False)

    _ <- labeled
      "Material"
      checkboxesInput
      (materialExample "cbx_e") { _inputConfig_status = constDyn $ InputError
                                  "Error"
                                }

    _ <- labeled
      "Disabled"
      checkboxesInput
      (materialExample "cbx_d") { _inputConfig_status = constDyn InputDisabled }

    _ <- labeled
      "Success"
      checkboxesInput
      (materialExample "cbx_s")
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }
    pure ()

  elClass "form" "vertical" $ do
    _ <- labeled "Terms and conditions"
                 (checkboxesInputMap (Map.singleton () (text "I agree")))
                 (inputConfig "cbx_agree" Set.empty)

    _ <- labeled "Vertical layout" checkboxesInput (materialExample "cbx_vert")

    pure ()

  el "h3" $ text "Indeterminate"
  el "form" $ do
    _ <- tricheckboxInput "Subscribe to the news letter"
                          (inputConfig "cbm" Nothing)
    pure ()

  el "h2" $ text "Toggle"
  el "form" $ do
    _ <- toggleInput "Turn it on" (inputConfig "tog_on" False)
    _ <- labeled "Material" togglesInput (materialExample "tog_n")
    _ <- labeled
      "Material"
      togglesInput
      (materialExample "tog_e") { _inputConfig_status = constDyn $ InputError
                                  "Error"
                                }
    _ <- labeled
      "Disabled"
      togglesInput
      (materialExample "tog_d") { _inputConfig_status = constDyn InputDisabled }
    _ <- labeled
      "Success"
      togglesInput
      (materialExample "togl_s")
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }
    pure ()

  pure never
  where materialExample idStr = materialExample' idStr Set.singleton

materialExample'
  :: (DomBuilder t m, DomBuilder t m1)
  => Text
  -> (Material -> f Material)
  -> InputConfig' (OpElem m1) t m (f Material)
materialExample' idStr bump =
  inputConfig' (OpElem printMaterial) idStr (bump M14307)

comboboxHandler
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender js t m
     )
  => m (Event t URI)
comboboxHandler = do
  el "h1" $ text "Combobox"
  el "form" $ do
    _ <- labeled
      "Flavours"
      (comboboxInput showOpt flavors)
      (inputConfig "cmb" def)
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Choose your favorite flavour")
        }
    _ <- labeled
      "Error"
      (comboboxInput showOpt flavors)
      (inputConfig "cmb_e" def)
        { _inputConfig_status = constDyn $ InputError "Error"
        }

    _ <- labeled
      "Disabled"
      (comboboxInput showOpt flavors)
      (inputConfig "cmb_d" def) { _inputConfig_status = constDyn InputDisabled }

    _ <- labeled
      "Success"
      (comboboxInput showOpt flavors)
      (inputConfig "cmb_s" def)
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }

    _ <- labeled "Material" altSelectInput (materialExample' "cmb_alt" Just)

    pure ()

  el "h2" $ text "Multi selection list"
  el "form" $ do
    _ <- labeled
      "Flavours"
      (multiComboboxInput showOpt flavors)
      (inputConfig "msel" def)
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Choose your favorite flavour")
        }
    _ <- labeled
      "Error"
      (multiComboboxInput showOpt flavors)
      (inputConfig "msel_e" def)
        { _inputConfig_status = constDyn $ InputError "Error"
        }

    _ <- labeled
      "Disabled"
      (multiComboboxInput showOpt flavors)
      (inputConfig "msel_d" def)
        { _inputConfig_status       = constDyn InputDisabled
        , _inputConfig_initialValue = ComboboxValue (Set.singleton 2) mempty
        }

    _ <- labeled
      "Success"
      (multiComboboxInput showOpt flavors)
      (inputConfig "msel_s" def)
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }

    pure never
 where
  showOpt k v = dynText ((<> " ") . pack . show <$> k) >> dynText v
  flavors = Map.fromList $ zip
    [(1 :: Integer), 2 ..]
    [ "Cherry"
    , "Mint chip"
    , "Vanilla"
    , "Lemon"
    , "Banana"
    , "Apple"
    , "Strawberry"
    , "Chocolate chip"
    ]

datalistHandler
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => m (Event t URI)
datalistHandler = do
  el "h1" $ text "Datalist"

  el "form" $ do
    _ <- labeled "Ice Cream Flavor"
                 (datalistInput flavors)
                 (inputConfig "dl" def)
    _ <- labeled
      "Ice Cream Flavor"
      (datalistInput flavors)
      (inputConfig "dl_n" def)
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Pick your favorite ice cream flavor")
        }
    _ <- labeled
      "Disabled"
      (datalistInput flavors)
      (inputConfig "dl_d" def) { _inputConfig_status = constDyn InputDisabled }
    _ <- labeled
      "Error"
      (datalistInput flavors)
      (inputConfig "dl_e" def)
        { _inputConfig_status = constDyn $ InputError "Unknown ice cream flavor"
        }
    _ <- labeled
      "Success"
      (datalistInput flavors)
      (inputConfig "dl_s" def)
        { _inputConfig_status = constDyn $ InputSuccess "Changes saved"
        }
    pure ()

  pure never

 where
  flavors = constDyn $ Map.fromList $ zip
    [(1 :: Integer) ..]
    ["Cherry", "Mint chip", "Vanilla", "Lemon"]

fileHandler
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     )
  => m (Event t URI)
fileHandler = do
  el "h1" $ text "File"

  el "form" $ do
    _ <- labeled "Attachment" fileInput (inputConfig "file" def)
    _ <- labeled
      "Attachment"
      fileInput
      (inputConfig "file_n" def)
        { _inputConfig_status = constDyn
                                  $ InputNeutral (Just "Max file size: 128 MB")
        }
    _ <- labeled
      "Disabled"
      fileInput
      (inputConfig "file_d" def) { _inputConfig_status = constDyn InputDisabled
                                 }
    _ <- labeled
      "Error"
      fileInput
      (inputConfig "file_e" def)
        { _inputConfig_status = constDyn $ InputError "Exceed file size limit"
        }
    _ <- labeled
      "Success"
      fileInput
      (inputConfig "file_s" def)
        { _inputConfig_status = constDyn $ InputSuccess "File accepted"
        }

    _ <- labeled
      "Dropzone"
      fileDropzone
      (inputConfig "file_drp" def)
        { _inputConfig_status = constDyn $ InputSuccess "File accepted"
        }

    pure ()

  pure never

groupHandler
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
groupHandler = do
  el "h1" $ text "Input Group"

  el "form" $ do
    labeled "Domain" (`inputGroup` content "grp") (inputConfig "grp" def)
    labeled
      "Domain"
      (`inputGroup` content "grp_n")
      (inputConfig "grp_n" def)
        { _inputConfig_status = constDyn
                                  $ InputNeutral (Just "Choose a domain name")
        }
    labeled
      "Disabled"
      (`inputGroup` content "grp_d")
      (inputConfig "grp_d" def) { _inputConfig_status = constDyn InputDisabled }
    labeled
      "Error"
      (`inputGroup` content "grp_e")
      (inputConfig "grp_e" def)
        { _inputConfig_status = constDyn $ InputError "Unreachable domain"
        }
    labeled
      "Success"
      (`inputGroup` content "grp_s")
      (inputConfig "grp_s" def)
        { _inputConfig_status = constDyn $ InputSuccess "Domain ready"
        }

  pure never

 where
  content idStr = do
    _ <- labeled
      "Protocol"
      selectInput
      (inputConfig' (OpElem printProtocol) (idStr <> "-p") (Just Http))
    _ <- labeled "Domain" textInput (inputConfig (idStr <> "-d") "")
    _ <- labeled
      "Toplevel"
      selectInput
      (inputConfig' (OpElem printDomain) (idStr <> "-t") (Just Dev))
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
      aceConfig
      "md"
      "# Markdown editor\n\n```haskell\nmain :: IO ()\nmain = pure ()\n```\n\nWith LaTeX: `$x^2$`"
    )
  signpost def markdownCheatSheet
  pure never

passwordHandler
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => m (Event t URI)
passwordHandler = do
  el "h1" $ text "Password"

  el "form" $ do
    _ <- labeled "Password" passwordInput (inputConfig "pw_n" def)
    _ <- labeled
      "Password"
      passwordInput
      (inputConfig "pw_nn" def)
        { _inputConfig_status = constDyn $ InputNeutral
          (Just
            "Use 8 or more characters with a mix of letters, numbers & symbols"
          )
        }
    _ <- labeled
      "Disabled"
      passwordInput
      (inputConfig "pw_d" def) { _inputConfig_status = constDyn InputDisabled }
    _ <- labeled
      "Error"
      passwordInput
      (inputConfig "pw_e" def)
        { _inputConfig_status = constDyn
          $ InputError "Use 8 or more characters for your password"
        }
    _ <- labeled
      "Success"
      passwordInput
      (inputConfig "pw_s" def)
        { _inputConfig_status = constDyn
                                  $ InputSuccess "Password meets requirements"
        }
    pure ()

  pure never

radioHandler :: (PostBuild t m, DomBuilder t m) => m (Event t URI)
radioHandler = do
  el "h1" $ text "Radio"

  el "form" $ do
    r1 <- labeled
      "Select material"
      radioInput
      (materialExample "rad_n")
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Select the material to be used")
        }
    r2 <- labeled
      "Error"
      radioInput
      (materialExample "rad_e") { _inputConfig_status = constDyn $ InputError
                                  "Error"
                                }

    r3 <- labeled
      "Disabled"
      radioInput
      (materialExample "rad_d") { _inputConfig_status = constDyn InputDisabled }

    r4 <- labeled
      "Success"
      radioInput
      (materialExample "rad_s")
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
  where materialExample idStr = materialExample' idStr Just

rangeHandler
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => m (Event t URI)
rangeHandler = do
  el "h1" $ text "Range"

  el "form" $ do
    _ <- labeled "Volume" rangeInput (inputConfig "range_n" (50 :: Double))
    _ <- labeled
      "Error"
      rangeInput
      (inputConfig "range_e" (50 :: Double))
        { _inputConfig_status = constDyn $ InputError "System error"
        }

    _ <- labeled
      "Disabled"
      rangeInput
      (inputConfig "range_d" (50 :: Double))
        { _inputConfig_status = constDyn InputDisabled
        }

    _ <- labeled
      "Success"
      rangeInput
      (inputConfig "range_s" (50 :: Double))
        { _inputConfig_status = constDyn $ InputSuccess "Changes saved"
        }
    pure ()

  pure never

selectHandler
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender js t m
     )
  => m (Event t URI)
selectHandler = do
  el "h1" $ text "Select"

  el "form" $ do
    _ <- labeled
      "Select material"
      selectInput
      (materialExample "sel")
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Select the material to be used")
        }
    _ <- labeled
      "Error"
      selectInput
      (materialExample "sel_e") { _inputConfig_status = constDyn $ InputError
                                  "Error"
                                }

    _ <- labeled
      "Disabled"
      selectInput
      (materialExample "sel_d") { _inputConfig_status = constDyn InputDisabled }

    _ <- labeled
      "Success"
      selectInput
      (materialExample "sel_s")
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }
    pure ()

  el "h2" $ text "Multiselect"
  el "form" $ do
    _ <- labeled
      "Select material"
      multiSelectInput
      (materialExamples "mselect_n")
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Choose your favorite flavour")
        }
    _ <- labeled
      "Error"
      multiSelectInput
      (materialExamples "mselect_e") { _inputConfig_status = constDyn
                                       $ InputError "Error"
                                     }

    _ <- labeled
      "Disabled"
      multiSelectInput
      (materialExamples "mselect_d") { _inputConfig_status = constDyn
                                       InputDisabled
                                     }

    _ <- labeled
      "Success"
      multiSelectInput
      (materialExamples "mselect_s")
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }
    pure ()

  pure never
 where
  materialExample idStr = materialExample' idStr Just
  materialExamples idStr = materialExample' idStr Set.singleton

textareaHandler :: (PostBuild t m, DomBuilder t m) => m (Event t URI)
textareaHandler = do
  el "h1" $ text "Textarea"

  el "form" $ do
    _ <- labeled
      "Select material"
      textAreaInput
      (inputConfig "area_n" def)
        { _inputConfig_status = constDyn
          $ InputNeutral (Just "Select the material to be used")
        }
    _ <- labeled
      "Error"
      textAreaInput
      (inputConfig "area_e" def)
        { _inputConfig_status = constDyn $ InputError "Error"
        }

    _ <- labeled
      "Disabled"
      textAreaInput
      (inputConfig "area_d" def) { _inputConfig_status = constDyn InputDisabled
                                 }

    _ <- labeled
      "Success"
      textAreaInput
      (inputConfig "area_s" def)
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }
    pure ()

  pure never

timeHandler
  :: (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m)
  => m (Event t URI)
timeHandler = do
  el "h1" $ text "Time"

  el "form" $ do
    t1 <- labeled "Time"
                  timeOfDayInput
                  (inputConfig "d_time_n" (Just (TimeOfDay 11 12 14)))
    _ <- labeled
      "Error"
      timeOfDayInput
      (inputConfig "d_time_e" def)
        { _inputConfig_status = constDyn $ InputError "Error"
        }

    _ <- labeled
      "Disabled"
      timeOfDayInput
      (inputConfig "d_time_d" def) { _inputConfig_status = constDyn
                                     InputDisabled
                                   }

    _ <- labeled
      "Success"
      timeOfDayInput
      (inputConfig "d_time_s" def)
        { _inputConfig_status = constDyn $ InputSuccess "Success message"
        }
    display (_inputEl_value t1)
    pure ()

  el "h2" $ text "Other date inputs"
  el "form" $ do
    d1 <- labeled "Day"
                  (requiredInput dateInput)
                  (inputConfig "d_day" (Just (fromGregorian 2021 03 14)))
    dt1 <- labeled
      "Local time"
      localtimeInput
      (inputConfig
        "d_localtime"
        (Just (LocalTime (fromGregorian 2021 03 14) (TimeOfDay 15 30 00)))
      )
    w1 <- labeled "Week" weekInput (inputConfig "d_week" (Just (2021, 30)))
    m1 <- labeled "Month" monthInput (inputConfig "d_mon" (Just (2021, 03)))
    display (_inputEl_value d1)
    text ", "
    display (_inputEl_value dt1)
    text ", "
    display (_inputEl_value w1)
    text ", "
    display (_inputEl_value m1)
    pure ()

  pure never
