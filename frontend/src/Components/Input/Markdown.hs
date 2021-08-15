{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Components.Input.Markdown
  ( codeInputScripts
  , codeInputStyle
  , codeInput
  , cdnAceConfig
  , markdownCheatSheet
  , markdownInputStyle
  , markdownInput
  , markdownInputWithPreview
  ) where

import           Clay                    hiding ( button
                                                , id
                                                , map
                                                , script
                                                )
import           Commonmark
import           Commonmark.Extensions
import           Commonmark.Pandoc
import           Control.Lens                   ( Const(..)
                                                , (^.)
                                                )
import           Control.Monad                  ( join
                                                , void
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Foldable                  ( toList )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as Text
import qualified GHCJS.DOM.Types               as DOM
import           Language.Javascript.JSaddle
import           Reflex.Dom
import           Reflex.Dom.Ace
import           Reflex.Dom.Pandoc
import           Text.Pandoc.Builder            ( Blocks
                                                , Inlines
                                                )
import           Text.Pandoc.Definition

import           Components.Input.Basic
import           Nordtheme                      ( grey0' )

codeInputScripts :: (DomBuilder t m) => m ()
codeInputScripts = do
  script "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.12/ace.min.js"
  script "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"
  where script uri = elAttr "script" ("src" =: uri) blank

cdnAceConfig :: Const AceConfig a
cdnAceConfig = Const $ def
  { _aceConfigBasePath        = Just
                                  "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.12/"
  , _aceConfigShowPrintMargin = True
  , _aceConfigWordWrap        = True
  }

codeInputStyle :: Css
codeInputStyle = do
  ".code-editor" Clay.** ".ace_editor" ? do
    fontFamily ["FiraCode", "FiraCode Nerd Font"] [monospace]
    fontSize (Clay.rem (14 / 16))
    minHeight (Clay.rem 10)
    height (pct 100 @-@ Clay.rem 2)
    borderRadius (px 3) (px 3) (px 3) (px 3)
    padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)

  ".code-editor" ? do
    "grid-column" -: "1/-1"


type CodeInputConfig = InputConfig' (Const AceConfig)

codeInput
  :: ( MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     , MonadFix m
     )
  => CodeInputConfig t m Text
  -> m (DomInputEl t m Text)
codeInput cfg = do

  dynAttrs <- holdDynAttributes
    (_inputConfig_attributes cfg <> "id" =: "editor")
    (_inputConfig_modifyAttributes cfg)

  (e, ())     <- elDynAttr' "div" dynAttrs (text initText)

  postBuildEv <- getPostBuild
  postBuild   <- delay 0.1 postBuildEv

  dynValDyn   <- prerender
    (pure $ constDyn triv)
    (widgetHold
      (pure triv)
      (  aceEditor (getConst $ _inputConfig_extra cfg)
                   readonlyDyn
                   initText
                   (_inputConfig_setValue cfg)
      <$ postBuild
      )
    )

  let dynVal = join dynValDyn

  pure $ InputEl
    { _inputEl_value    = fst =<< dynVal
    , _inputEl_hasFocus = (&&)
                          <$> (snd =<< dynVal)
                          <*> (Prelude.not <$> readonlyDyn)
    , _inputEl_elements = constDyn [e]
    }

 where
  triv        = (constDyn initText, constDyn False)
  initText    = _inputConfig_initialValue cfg
  readonlyDyn = mkReadonly <$> _inputConfig_status cfg
  mkReadonly InputDisabled = True
  mkReadonly _             = False


aceEditor
  :: ( DomBuilder t m
     , PostBuild t m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadHold t m
     )
  => AceConfig
  -> Dynamic t Bool
  -> Text
  -> Event t Text
  -> m (Dynamic t Text, Dynamic t Bool)
aceEditor aceCfg readonlyDyn initText setTextEv = do
  (ace, focusDyn) <- aceWidget' aceCfg
                                (AceDynConfig Nothing)
                                never
                                "editor"
                                initText
                                setTextEv
  postBuild <- getPostBuild
  _ <- withAceInstance ace (applyInit <$> tag (current readonlyDyn) postBuild)
  _ <- withAceInstance ace (applyReadonly <$> updated readonlyDyn)

  pure (aceValue ace, focusDyn)
 where
  applyReadonly rdOnly (AceInstance ace) =
    liftJSM $ void $ ace ^. js1 ("setReadOnly" :: Text) rdOnly

  applyInit rdOnly (AceInstance ace) = liftJSM $ do
    void $ ace ^. js1 ("setTheme" :: Text) ("ace/theme/nord_dark" :: Text)
    void $ ace ^. js2 ("setOption" :: Text) ("maxLines" :: Text) (50 :: Double)
    void $ ace ^. js1 ("setReadOnly" :: Text) rdOnly
    session <- ace ^. js0 ("getSession" :: Text)
    void $ session ^. js1 ("setTabSize" :: Text) (2 :: Double)
    void $ session ^. js1 ("setUseSoftTabs" :: Text) True
    void $ session ^. js1 ("setNavigateWithinSoftTabs" :: Text) True

aceWidget'
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadJSM m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     )
  => AceConfig -- ^ Ace editor configurations
  -> AceDynConfig -- ^ Ace editor theme
  -> Event t AceDynConfig -- ^ Updatable Ace editor theme
  -> Text -- ^ ID of desired container element
  -> Text -- ^ Initial Ace editor contents
  -> Event t Text -- ^ Updatable Ace editor contents
  -> m (Ace t, Dynamic t Bool)
aceWidget' ac adc adcUps containerId initContents contentsUps = do
  aceInstance <- startAce containerId ac
  onChange    <- setupValueListener aceInstance
  updatesDyn  <- holdDyn initContents onChange

  let ace = Ace (constDyn $ pure aceInstance) updatesDyn
  setThemeAce (_aceDynConfigTheme adc) aceInstance
  void $ withAceInstance ace (setThemeAce . _aceDynConfigTheme <$> adcUps)
  performEvent_ $ ffor contentsUps $ \c -> setValueAce c aceInstance

  focusEv  <- setupFocusListener aceInstance "focus"
  blurEv   <- setupFocusListener aceInstance "blur"
  focusDyn <- holdDyn False (leftmost [True <$ focusEv, False <$ blurEv])

  return (ace, focusDyn)

setupFocusListener
  :: ( MonadJSM (Performable m)
     , DomBuilder t m
     , PostBuild t m
     , TriggerEvent t m
     , PerformEvent t m
     )
  => AceInstance
  -> Text
  -> m (Event t ())
setupFocusListener (AceInstance ace) evName = do
  pb <- getPostBuild
  let act cb = liftJSM $ do
        jscb <- asyncFunction $ \_ _ _ -> liftIO (cb ())
        void $ ace ^. js2 ("on" :: Text) evName jscb
  performEventAsync (act <$ pb)

markdownInputStyle :: Css
markdownInputStyle = do
  skylightingStyle
  ".code-view" ? do
    border solid 1 grey0'
    borderRadius (px 3) (px 3) (px 3) (px 3)
    padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    boxShadow . pure $ bsColor grey0' $ shadowWithBlur nil
                                                       (Clay.rem (1 / 8))
                                                       nil

  ".code-input" ? do
    Clay.display grid
    "grid-gap" -: "1rem"
    "grid-template-columns" -: "repeat(auto-fit, minmax(30rem, 1fr))"
    "grid-template-rows" -: "1fr auto"

    ".statusmessage" ? do
      "grid-row" -: "2"

markdownCheatSheet :: (DomBuilder t m) => m ()
markdownCheatSheet = do
  el "h4" $ text "Markdown cheat sheet"

  el "pre" $ el "code" $ do
    text $ Text.unlines
      [ "# Heading 1"
      , "## Heading 2"
      , "**Bold text**"
      , "*Italicized text*"
      , "[Link title](https://example.com)"
      , "![Image alt text](image.jpg)"
      , "> blockquote"
      , "1. Ordered list"
      , "- Unordered list"
      ]

parseMarkdownImproving
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Dynamic t Text
  -> m (Dynamic t InputStatus, Dynamic t (Maybe (Dynamic t Pandoc)))
parseMarkdownImproving textD = do
  let dynMarkWithError = parseMarkdown <$> textD
  mDynMark <- improvingMaybe (either (const Nothing) pure <$> dynMarkWithError)
  let dynError = either (InputError . pack . show) def <$> dynMarkWithError

  dynMark <- maybeDyn mDynMark
  pure (dynError, dynMark)
 where

  ext =
    mathSpec
      <> footnoteSpec
      <> smartPunctuationSpec
      <> gfmExtensions
      <> defaultSyntaxSpec
  parseMarkdown =
    fmap (Pandoc mempty . toList . unCm)
      . runIdentity
      . commonmarkWith @Identity @(Cm () Inlines) @(Cm () Blocks) ext ""

markdownInput
  :: ( MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , Prerender js t m
     , MonadFix m
     )
  => CodeInputConfig t m Text
  -> m (DomInputEl t m Text)
markdownInput cfg = elClass "div" "code-editor" $ codeInput
  (cfg
    { _inputConfig_extra = Const $ (getConst (_inputConfig_extra cfg))
                             { _aceConfigMode = Just "markdown"
                             }
    }
  )


markdownInputWithPreview
  :: ( MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , Prerender js t m
     )
  => SyntaxMap
  -> CodeInputConfig t m Text
  -> m (DomInputEl t m Text)
markdownInputWithPreview syntaxMap cfg = elClass "div" "code-input" $ do
  textDEl <- markdownInput cfg
  let textD = _inputEl_value textDEl
  (dynError, dynMark) <- parseMarkdownImproving textD

  _                   <- elClass "div" "code-view" $ dyn
    (   maybe (pure (constDyn ()))
              (elPandoc defaultConfig { _config_syntaxMap = syntaxMap })
    <$> dynMark
    )

  statusMessageDiv (dynError <> _inputConfig_status cfg)

  delayEv <- delay 0.05 (updated textD)
  _ <- prerender_ blank $ performEvent (mathJaxTypeset <$ delayEv) >> pure ()
  pure textDEl
 where
  mathJaxTypeset = DOM.liftJSM $ do
    mathjax <- jsg ("MathJax" :: Text)
    mathjax ^. js0 ("typeset" :: Text)


skylightingStyle :: Css
skylightingStyle = do
  ".source-code" ? do -- Normal
    padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    backgroundColor (parse "#2e3440")
    color (parse "#d8dee9")
  ".al" ? do -- Alert
    backgroundColor (parse "#3b4252")
    fontWeight (weight 500)
    color (parse "#bf616a")
  ".an" ? do -- Annotation
    color (parse "#d08770")
  ".at" ? do -- Attribute
    color (parse "#8fbcbb")
  ".bn" ? do -- BaseN
    color (parse "#b48ead")
  ".bu" ? do -- BuiltIn
    fontStyle italic
    color (parse "#88c0d0")
  ".ch" ? do -- Char
    color (parse "#ebcb8b")
  ".co" ? do -- Comment
    color (parse "#616e88")
  ".cv" ? do -- CommentVar
    color (parse "#e5e9f0")
  ".cn" ? do -- Constant
    fontWeight (weight 500)
    color (parse "#eceff4")
  ".cf" ? do -- ControlFlow
    fontWeight (weight 500)
    color (parse "#81a1c1")
  (".dt" <> ".cr") ? do -- DataType
    color (parse "#81a1c1")
  (".dv" <> ".it") ? do -- DecVal
    color (parse "#b48ead")
  ".do" ? do -- Documentation
    color (parse "#5e81ac")
  ".er" ? do -- Error
    color (parse "#bf616a")
    textDecoration underline
  ".ex" ? do -- Extension
    fontWeight (weight 500)
    color (parse "#8fbcbb")
  (".fl" <> ".ra") ? do -- Float
    color (parse "#b48ead")
  ".fu" ? do -- Function
    color (parse "#88c0d0")
  ".im" ? do -- Import
    color (parse "#a3be8c")
  ".in" ? do -- Information
    color (parse "#ebcb8b")
  (".kw" <> ".sy") ? do -- Keyword
    fontWeight (weight 500)
    color (parse "#81a1c1")
  ".op" ? do -- Operator
    color (parse "#81a1c1")
  ".ot" ? do -- Others
    color (parse "#8fbcbb")
  (".pp" <> ".pr") ? do -- Preprocessor
    color (parse "#5e81ac")
  ".re" ? do -- RegionMarker
    backgroundColor (parse "#3b4252")
    color (parse "#88c0d0")
  ".sc" ? do -- SpecialChar
    color (parse "#ebcb8b")
  ".ss" ? do -- SpecialString
    color (parse "#d08770")
  ".st" ? do -- String
    color (parse "#a3be8c")
  ".va" ? do -- Variable
    color (parse "#5e81ac")
  ".vs" ? do -- VerbatimString
    color (parse "#a3be8c")
  ".wa" ? do -- Warning
    color (parse "#bf616a")

