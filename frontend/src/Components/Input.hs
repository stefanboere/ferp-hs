{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Components.Input
  ( numberInput
  , inputStyle
  , InputConfig(..)
  , inputElement'
  , InputStatus(..)
  )
where

import           Clay                    hiding ( (&)
                                                , max
                                                )
import           Clay.Stylesheet                ( key )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Default
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Reflex
import           Reflex.Dom
import           Text.Read                      ( readMaybe )
import           Text.Printf                    ( printf )
import           System.Random                  ( getStdGen
                                                , StdGen
                                                , setStdGen
                                                , random
                                                )

import           Nordtheme

data InputStatus = InputNeutral (Maybe Text) | InputSuccess Text | InputError Text | InputDisabled deriving (Eq, Ord)

instance Default InputStatus where
  def = InputNeutral Nothing

data InputConfig t a = InputConfig
  { _inputConfig_initialValue :: a
  , _inputConfig_setValue     :: Event t a
  , _inputConfig_label        :: Dynamic t Text
  , _inputConfig_status       :: Dynamic t InputStatus
  , _inputConfig_attributes   :: Map AttributeName Text
  }

instance (Reflex t) => Default (InputConfig t Text) where
  def = InputConfig { _inputConfig_initialValue = ""
                    , _inputConfig_setValue     = never
                    , _inputConfig_label        = constDyn ""
                    , _inputConfig_status       = def
                    , _inputConfig_attributes   = def
                    }

instance (Default a, Reflex t) => Default (InputConfig t a) where
  def = InputConfig { _inputConfig_initialValue = def
                    , _inputConfig_setValue     = never
                    , _inputConfig_label        = constDyn ""
                    , _inputConfig_status       = def
                    , _inputConfig_attributes   = def
                    }

inputStyle :: Css
inputStyle = do
  input ? do
    background transparent
    borderWidth 0
    borderBottomWidth 1
    padding (px 4) (px 4) (px 4) (px 4)
    borderColor nord4'
    outlineWidth 0

  input # focus ? do
    borderBottomWidth 2
    borderColor nord10'
    marginBottom (px (-1))

  input # "::placeholder" ? do
    fontColor nord4'

  input # disabled ? do
    fontColor nord4'

  input # ".has-error" ? do
    borderColor nord11'

  input # ".has-success" ? do
    borderColor nord14'

  label ? do
    fontWeight bold
    lineHeight (px 23)

  form ? do
    Clay.display grid
    key "grid-column-gap"       (px 50)
    key "grid-template-columns" (pct 25, pct 75)

    ".helptext" ? do
      fontSize (px 12)

    ".helptext" # ".has-error" ? do
      fontColor nord11'

    ".helptext" # ".has-success" ? do
      fontColor nord14'

inputElement'
  :: (PostBuild t m, DomBuilder t m, MonadIO m)
  => InputConfig t Text
  -> m (Dynamic t Text)
inputElement' cfg = do
  stdGen <- liftIO getStdGen
  let (idInt, stdGen') = random stdGen :: (Int, StdGen)
  liftIO $ setStdGen stdGen'
  let idStr = pack (printf "%x" idInt)


  elAttr "label" ("for" =: idStr) $ dynText (_inputConfig_label cfg)

  postBuildEv <- getPostBuild
  let postBuildAttrEv = attachPromptlyDynWith
        (\x _ -> Just <$> statusAttrs x)
        (_inputConfig_status cfg)
        postBuildEv

  el "div" $ do
    n <-
      inputElement
      $  def
      &  inputElementConfig_initialValue
      .~ _inputConfig_initialValue cfg
      &  inputElementConfig_setValue
      .~ _inputConfig_setValue cfg
      &  inputElementConfig_elementConfig
      .  elementConfig_initialAttributes
      .~ _inputConfig_attributes cfg
      <> "id"
      =: idStr
      &  inputElementConfig_elementConfig
      .  elementConfig_modifyAttributes
      .~ leftmost [modAttrEv, postBuildAttrEv]

    let dynClass = (\state -> "class" =: (colorCls state <> " helptext"))
          <$> _inputConfig_status cfg
    let dynMessage = message <$> _inputConfig_status cfg
    elDynAttr "div" dynClass $ dynText dynMessage

    pure (_inputElement_value n)

 where
  modAttrEv = updated (fmap Just . statusAttrs <$> _inputConfig_status cfg)
  statusAttrs :: InputStatus -> Map AttributeName Text
  statusAttrs state = "class" =: colorCls state <> if state == InputDisabled
    then "disabled" =: ""
    else Map.empty

  colorCls (InputSuccess _) = "has-success"
  colorCls (InputError   _) = "has-error"
  colorCls _                = ""

  message (InputError   x       ) = x
  message (InputSuccess x       ) = x
  message (InputNeutral (Just x)) = x
  message _                       = ""

numberInput
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , Show a
     , MonadIO m
     )
  => InputConfig t a
  -> m (Dynamic t (Maybe a))
numberInput cfg = do
  let initAttrs = ("type" =: "number") <> _inputConfig_attributes cfg
      styleChange :: Maybe a -> InputStatus
      styleChange result = case result of
        (Just _) -> def
        Nothing  -> InputError "Not a valid number"

  rec
    n <- inputElement' cfg
      { _inputConfig_initialValue = pack . show $ _inputConfig_initialValue cfg
      , _inputConfig_setValue     = pack . show <$> _inputConfig_setValue cfg
      , _inputConfig_status = zipDynWith max (_inputConfig_status cfg) statusDyn
      , _inputConfig_attributes   = _inputConfig_attributes cfg <> initAttrs
      }
    let result    = readMaybe . unpack <$> n
        modAttrEv = fmap styleChange (updated result)
    statusDyn <- holdDyn def modAttrEv
  return result

