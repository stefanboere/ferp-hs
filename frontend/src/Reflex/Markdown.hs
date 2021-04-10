{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Markdown
  ( codeInputScripts
  , whenLoaded
  , codeInputStyle
  , codeInput
  , markdownInputStyle
  , markdownInput
  )
where

import           Clay                    hiding ( button
                                                , id
                                                , map
                                                , script
                                                )
import           Control.Lens                   ( (^.) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Monoid                    ( All(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Language.Javascript.JSaddle
import           Language.Javascript.JSaddle.Types
                                                ( MonadJSM )
import           Reflex.CodeMirror
import           Reflex.Dom
import           Reflex.Dom.MMark
import qualified Text.MMark                    as MMark
import qualified Text.Megaparsec               as M

import           GHCJS.DOM.Element              ( IsElement )
import qualified GHCJS.DOM.Types               as DOM

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
  minHeight (Clay.rem 10)
  height (pct 100 @-@ Clay.rem 2)
  borderRadius (px 3) (px 3) (px 3) (px 3)
  padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)


codeInput
  :: ( MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     , TriggerEvent t m
     , MonadJSM m
     , PerformEvent t m
     , MonadIO (Performable m)
     , IsElement (RawElement (DomBuilderSpace m))
     )
  => Configuration
  -> Text
  -> Event t Text
  -> m (Dynamic t Text)
codeInput cfg initText setValueEv = do
  postBuildEv      <- getPostBuild
  postBuildEvDelay <- delay 0.05 postBuildEv
  let initTextEv = initText <$ postBuildEvDelay
  let outsideEv  = leftmost [setValueEv, initTextEv]

  textEv <- codemirror cfg { _configuration_value = Just initText }
                       outsideEv
                       never

  holdDyn initText (leftmost [textEv, setValueEv])

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


markdownInput
  :: ( MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , TriggerEvent t m
     , MonadJSM m
     , PerformEvent t m
     , MonadIO (Performable m)
     , IsElement (RawElement (DomBuilderSpace m))
     , MonadJSM (Performable m)
     )
  => SyntaxMap
  -> InputConfig t Text
  -> m
       ( Dynamic
           t
           ( Either
               (M.ParseErrorBundle Text MMark.MMarkErr)
               MMark.MMark
           )
       )
markdownInput syntaxMap cfg = elClass "div" "code-input" $ do
  textD <- elClass "div" "code-editor"
    $ codeInput
        config
        (_inputConfig_initialValue cfg)
        (_inputConfig_setValue cfg)

  let dynMMarkWithError = MMark.parse "" <$> textD
  dynMark <- improvingMaybe (either (const Nothing) pure <$> dynMMarkWithError)
  let dynError =
        either (InputError . pack . M.errorBundlePretty) def
          <$> dynMMarkWithError

  elClass "div" "code-view" $ renderDom syntaxMap dynMark

  statusMessageDiv (dynError <> _inputConfig_status cfg)

  delayEv <- delay 0.05 (updated textD)
  _       <- performEvent (mathJaxTypeset <$ delayEv)
  pure dynMMarkWithError
 where
  config :: Configuration
  config = def { _configuration_theme          = Just "nord"
               , _configuration_indentUnit     = Just 2
               , _configuraiton_smartIndent    = Just True
               , _configuration_tabSize        = Just 2
               , _configuration_indentWithTabs = Just False
               }
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

