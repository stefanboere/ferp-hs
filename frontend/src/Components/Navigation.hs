{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Components.Navigation
  ( HeaderConfig(..)
  , app
  , appStyle
  )
where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( icon )
import           Data.Default
import           Data.Text                      ( Text )
import           Reflex.Dom

import           Components.Class
import           Components.Icon
import           Nordtheme


appStyle :: Css
appStyle = do
  appHeaderStyle

  body ? do
    marginAll nil
    background white0'
    fontColor nord3'
    fontFamily ["Fira Sans", "Helvetica"] [sansSerif]

  main_ ? do
    marginAll (rem 0.5)



app :: (PostBuild t m, DomBuilder t m) => HeaderConfig t -> m () -> m ()
app cfg page = do
  appHeader cfg
  el "main" page


newtype HeaderConfig t = HeaderConfig
  { _headerConfig_appname :: Dynamic t Text
  }

instance Reflex t => Default (HeaderConfig t) where
  def = HeaderConfig { _headerConfig_appname = constDyn "" }

appHeaderStyle :: Css
appHeaderStyle = do
  ".app-icon" ? do
    verticalAlign middle
    paddingRight (rem 0.5)

  ".app-header" ? do
    height (rem 3)
    overflow hidden
    background nord0'
    fontColor nord6'

    a ? do
      fontColor nord6'
      fontSize (rem 1.2)
      textDecoration none
      paddingAll (rem 1)
      lineHeight (rem 3)

      hover Clay.& do
        background nord2'

  ".app-logo" ? do
    important $ backgroundColor inherit

appHeader :: (PostBuild t m, DomBuilder t m) => HeaderConfig t -> m ()
appHeader HeaderConfig {..} = elClass "header" "app-header" $ do
  elAttr "a" ("href" =: "/" <> "class" =: "app-logo") $ do
    icon def { _iconConfig_size = 2, _iconConfig_class = Just "app-icon" }
         ferpIcon
    dynText _headerConfig_appname
