{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Reflex.Markdown
  ( codeInputScripts
  , whenLoaded
  , codeInputStyle
  , codeInput
  , markdownInputStyle
  , markdownInput
  , markdownInputWithPreview
  )
where

import           Clay                    hiding ( button
                                                , id
                                                , map
                                                , script
                                                )
import           Commonmark
import           Commonmark.Extensions
import           Commonmark.Pandoc
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Foldable                  ( toList )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Monoid                    ( All(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified GHCJS.DOM.Types               as DOM
import           Language.Javascript.JSaddle
import           Control.Lens                   ( (^.) )
import           Reflex.CodeMirror
import           Reflex.Dom
import           Reflex.Dom.Pandoc
import           Text.Pandoc.Builder            ( Blocks
                                                , Inlines
                                                )
import           Text.Pandoc.Definition

import           Components.Input.Basic
import           Nordtheme                      ( grey0' )

whenLoaded
  :: ( MonadHold t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => [Dynamic t Bool] -- ^ Wait for these to load
  -> m () -- ^ Loading widget
  -> m a -- ^ Loaded widget
  -> m (Dynamic t (Maybe a))
whenLoaded loadedDs loadingWidget loadedWidget = do
  allLoadedD <- pure . fmap getAll . mconcat $ map (fmap All) loadedDs
  let allLoadedE = ffilter id . updated $ allLoadedD
  delayedLoadedE <- delay 0.5 allLoadedE
  changeD        <-
    widgetHold (Nothing <$ loadingWidget) $ ffor delayedLoadedE $ \_ -> do
      Just <$> loadedWidget
  changeE_ <- headE $ updated changeD
  holdDyn Nothing changeE_


script
  :: (MonadHold t m, DomBuilder t m)
  => Text -- ^ URI
  -> m (Dynamic t Bool)
script uri = do
  (element_, _) <- elAttr' "script" ("src" =: uri) blank
  let loadedE = True <$ domEvent Load element_
  holdDyn False loadedE

css
  :: (MonadHold t m, DomBuilder t m)
  => Text -- ^ URI
  -> m (Dynamic t Bool)
css uri = do
  (element_, _) <- elAttr'
    "link"
    ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: uri)
    blank
  let loadedE = True <$ domEvent Load element_
  holdDyn False loadedE



codeInputScripts
  :: ( MonadHold t m
     , DomBuilder t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => m (Dynamic t Bool)
codeInputScripts = do
  s1Ds <- sequence
    [ script
      "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.js"
    , css
      "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.css"
    , css
      "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/theme/nord.css"
    , script "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"
    ]
  x <- whenLoaded s1Ds blank $ sequence_
    [ script
        "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/mode/markdown/markdown.min.js"
    ]
  pure ((== Just ()) <$> x)

codeInputStyle :: Css
codeInputStyle = ".code-editor" Clay.** ".CodeMirror" ? do
  fontFamily ["FiraCode", "FiraCode Nerd Font"] [monospace]
  minHeight (Clay.rem 10)
  height (pct 100 @-@ Clay.rem 2)
  borderRadius (px 3) (px 3) (px 3) (px 3)
  padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)


codeInput
  :: ( MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , Prerender js t m
     )
  => Configuration
  -> Text
  -> Event t Text
  -> m (DomInputEl t m Text)
codeInput cfg initText setValueEv = do
  postBuildEv      <- getPostBuild
  postBuildEvDelay <- delay 0.05 postBuildEv
  let initTextEv = initText <$ postBuildEvDelay
  let outsideEv  = leftmost [setValueEv, initTextEv]

  dynTextEv <- prerender (pure never)
    $ codemirror cfg { _configuration_value = Just initText } outsideEv never

  v <- holdDyn initText (leftmost [switchDyn dynTextEv, setValueEv])
  pure (hiddenInput v) -- FIXME add element and hasFocus

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
     )
  => InputConfig t Text
  -> m (DomInputEl t m Text)
markdownInput cfg = elClass "div" "code-editor"
  $ codeInput config (_inputConfig_initialValue cfg) (_inputConfig_setValue cfg)

 where
  config :: Configuration
  config = def { _configuration_theme          = Just "nord"
               , _configuration_indentUnit     = Just 2
               , _configuraiton_smartIndent    = Just True
               , _configuration_tabSize        = Just 2
               , _configuration_indentWithTabs = Just False
               }

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
  -> InputConfig t Text
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

