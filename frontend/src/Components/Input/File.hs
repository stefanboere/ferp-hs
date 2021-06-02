{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Components.Input.File
  ( fileDropzone
  , fileDropzoneStyle
  , fileDropzoneScript
  )
where


import           Prelude                 hiding ( rem )

import           Clay                    hiding ( (&)
                                                , icon
                                                , not
                                                )
import qualified Clay                           ( (&) )
import           Control.Monad                  ( (>=>) )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Default
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           GHCJS.DOM.File                 ( getName )
import qualified GHCJS.DOM.Types               as DOM
                                                ( File )
import           Reflex
import           Reflex.Dom              hiding ( fileInput )

import           Components.Icon
import           Components.Input.Basic
import           Nordtheme


fileDropzoneStyle :: Css
fileDropzoneStyle = ".dropzone" ? do
  borderStyle solid
  fontWeight (weight 600)
  justifyContent center

  ".file-upload-label" Clay.& backgroundColor white

  ".hover" Clay.& do
    borderColor green1'
    backgroundColor nord6'


fileDropzone
  :: ( PostBuild t m
     , DomBuilder t m
     , Prerender js t m
     , MonadFix m
     , MonadHold t m
     )
  => InputConfig t ()
  -> m (Dynamic t [DOM.File])
fileDropzone cfg = do
  modAttrEv <- statusModAttrEv' cfg

  x         <- elClass "div" "input" $ do
    rec
      (e, n) <-
        elDynAttr' "label" (buildAttrs <$> hoverDyn <*> _inputConfig_status cfg)
          $ do
              icon def folderIcon
              el "span" (text "Browse")
              inputElement
                $  def
                &  inputElementConfig_elementConfig
                .  elementConfig_initialAttributes
                .~ (_inputConfig_attributes cfg <> initAttrs)
                <> maybe mempty (Map.singleton "id") (_inputConfig_id cfg)
                &  inputElementConfig_elementConfig
                .  elementConfig_modifyAttributes
                .~ mergeWith (<>) [modAttrEv, _inputConfig_modifyAttributes cfg]

      let dragoverEv  = domEvent Dragover e
      let dropEv      = domEvent Drop e
      let dragleaveEv = domEvent Dragleave e
      hoverDyn <- holdDyn
        False
        (leftmost [True <$ dragoverEv, False <$ dropEv, False <$ dragleaveEv])

    statusMessageDiv (_inputConfig_status cfg)

    pure $ _inputElement_files n

  _ <- el "ul" $ simpleList x $ \f ->
    dyn ((getName' >=> el "li" . dynText) <$> f)

  pure x

 where
  buildAttrs h x = Map.fromList
    [ ( "class"
      , "file-upload-label secondary dropzone" <> if h
        then " hover"
        else "" <> if x == InputDisabled then " disabled" else mempty
      )
    , ("ondragover", "dragOverHandler(event);")
    , ( "ondrop"
      , "dropHandler('" <> fromMaybe "" (_inputConfig_id cfg) <> "', event);"
      )
    ]

  initAttrs = "type" =: "file" <> "multiple" =: ""

  getName' f = prerender (pure "?") (getName f)

-- | Small snippet of javascript to place the dropped files into the file input
fileDropzoneScript :: DomBuilder t m => m ()
fileDropzoneScript =
  el "script"
    $ text
        "function dragOverHandler(ev) { \
  \   ev.stopPropagation(); \
  \   ev.preventDefault(); \
  \ } \
  \ function dropHandler(idStr, ev) { \
  \   var fileInput = document.getElementById(idStr); \
  \   fileInput.files = ev.dataTransfer.files; \
  \   const e = new Event('change'); \
  \   fileInput.dispatchEvent(e); \
  \   ev.stopPropagation(); \
  \   ev.preventDefault(); \
  \ } \
  \ "
