{-# LANGUAGE OverloadedStrings #-}
module Components.Tag
  ( TagConfig(..)
  , TagColor(..)
  , TagAction(..)
  , tagStyle
  , tagEl
  , badge
  )
where

import           Prelude                 hiding ( rem )

import           Control.Monad                  ( when )
import           Clay                    hiding ( icon )
import           Data.Default
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Reflex.Dom              hiding ( display
                                                , tag
                                                , (&)
                                                )

import           Components.Class
import           Components.Icon
import           Nordtheme

data TagColor = TagStatusColor Status -- ^ Nord 10, 11, 13, 14
              | TagGrey
              | TagPurple -- ^ Nord 15
              | TagOrange -- ^ Nord 12
              | TagLightGreen -- ^ Nord 7
              | TagCyan -- ^ Nord 8
              | TagLightBlue -- ^ Nord 9
              deriving (Eq, Show)

instance Default TagColor where
  def = TagGrey

data TagAction = TagClick | TagDismiss deriving (Eq, Show)

data TagConfig t = TagConfig
  { _tagConfig_badge :: Maybe (Dynamic t Integer)
  , _tagConfig_action :: Maybe TagAction
  , _tagConfig_color :: TagColor
  }

instance Default (TagConfig t) where
  def = TagConfig { _tagConfig_badge  = def
                  , _tagConfig_action = def
                  , _tagConfig_color  = def
                  }

tagStyle :: Css
tagStyle = tagStyle' <> badgeStyle

tagStyle' :: Css
tagStyle' = ".tag" ? do
  borderRadiusAll (rem (5 / 8))
  border solid (px 1) nord3'
  padding (rem (1 / 8)) (rem (1 / 2)) (rem (1 / 8)) (rem (1 / 2))
  marginRight (rem (1 / 2))
  whiteSpace nowrap

  ".clickable" & do
    cursor pointer
    hover & do
      backgroundColor nord6'

  ".icon" ? do
    "fill" -: showColor nord3'
    position relative
    top (rem (1 / 4))

  ".badge" ? do
    marginLeft (rem (2 / 8))
    marginRight (rem (-3 / 8))

  ".tagpurple" & borderColor nord15'
  ".tagorange" & borderColor nord12'
  ".taglightgreen" & borderColor nord7'
  ".tagcyan" & borderColor nord8'
  ".taglightblue" & borderColor nord9'


tagEl
  :: (PostBuild t m, DomBuilder t m)
  => TagConfig t
  -> Dynamic t Text
  -> m (Event t ())
tagEl cfg lbl = do
  (e, _) <- elClass' "span" classStr $ do
    dynText lbl
    when (_tagConfig_action cfg == Just TagDismiss) $ icon def timesIcon
    maybe (pure ()) (badge (_tagConfig_color cfg)) $ _tagConfig_badge cfg
  text " "
  pure $ domEvent Click e

 where
  classStr =
    Text.toLower
      . Text.unwords
      . Prelude.filter (Prelude.not . Text.null)
      $ [ "tag"
        , case _tagConfig_action cfg of
          Just _  -> "clickable"
          Nothing -> ""
        , colorCls (_tagConfig_color cfg)
        ]
  colorCls (TagStatusColor c) = Text.pack . show $ c
  colorCls c                  = Text.pack . show $ c

badgeStyle :: Css
badgeStyle = ".badge" ? do
  borderRadiusAll (rem (1 / 2))
  padding (rem (1 / 16)) (rem (1 / 3)) (rem (1 / 16)) (rem (1 / 3))
  marginRight (rem (1 / 2))
  backgroundColor nord3'

  ".taggrey" & fontColor nord6'

  ".tagpurple" & do
    fontColor nord6'
    backgroundColor nord15'

  ".tagorange" & do
    fontColor nord6'
    backgroundColor nord12'

  ".taglightgreen" & backgroundColor nord7'

  ".tagcyan" & backgroundColor nord8'

  ".taglightblue" & do
    fontColor nord6'
    backgroundColor nord9'

badge
  :: (PostBuild t m, DomBuilder t m, Integral a, Show a)
  => TagColor
  -> Dynamic t a
  -> m ()
badge colr num = elClass "span"
                         ("badge " <> classStr)
                         (dynText (dynVal <$> num))
 where
  classStr = Text.toLower . Text.pack . show $ colr
  dynVal x | x >= 100  = "99+"
           | x < 0     = "<0"
           | otherwise = Text.pack $ show x

