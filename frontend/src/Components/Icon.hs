{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
   Clarity Design System Icons
-}
module Components.Icon
  ( Status(..)
  , Direction(..)
  , IconConfig(..)
  , icon
  -- * Core icons
  , successStandardIcon
  , errorStandardIcon
  , warningStandardIcon
  , infoStandardIcon
  , statusStandardIcon
  , checkCircleIcon
  , exclamationCircleIcon
  , angleIcon
  )
where

import           Clay                           ( Color
                                                , value
                                                , unValue
                                                , plain
                                                )
import           Data.Default
import           Data.Map                       ( Map )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Reflex.Dom              hiding ( value )

import           Nordtheme

data Status = Danger -- ^ Show errors to make user pause and evaluate
            | Warning  -- ^ Proceed with caution
            | Info  -- ^ Inform the user
            | Success -- ^ Let the user know something is correct
            deriving (Eq, Ord)

instance Default Status where
  def = Info

data Direction = DirUp  -- ^ No rotation
               | DirRight -- ^ 90 degrees rotation (right angle :p)
               | DirDown -- ^ 180 degrees rotation
               | DirLeft -- ^ 270 degrees rotation
               deriving (Eq, Enum, Bounded)

instance Default Direction where
  def = DirUp

statusColor :: Status -> Color
statusColor = \case
  Danger  -> nord11'
  Warning -> nord13'
  Info    -> nord10'
  Success -> green1'

showColor :: Color -> Text
showColor = plain . unValue . value

data IconConfig t = IconConfig
  { _iconConfig_size :: Int
  , _iconConfig_status :: Dynamic t (Maybe Status)
  , _iconConfig_direction :: Dynamic t Direction
  }

instance Reflex t => Default (IconConfig t) where
  def = IconConfig { _iconConfig_size      = 16
                   , _iconConfig_status    = def
                   , _iconConfig_direction = def
                   }

icon :: (PostBuild t m, DomBuilder t m) => IconConfig t -> m a -> m a
icon IconConfig {..} = elDynAttr
  "div"
  (   (\s d -> "class" =: "icon" <> "style" =: style s d)
  <$> _iconConfig_status
  <*> _iconConfig_direction
  )
 where
  style status direction =
    "display:inline-block;position:absolute;width:"
      <> rem'
      <> "rem;height:"
      <> rem'
      <> "rem;"
      <> styleStatus status
      <> styleDirection direction
  styleStatus Nothing  = ""
  styleStatus (Just x) = "fill:" <> showColor (statusColor x) <> ";"

  styleDirection DirUp    = mempty
  styleDirection DirRight = "transform:rotate(90deg);"
  styleDirection DirDown  = "transform:rotate(180deg);"
  styleDirection DirLeft  = "transform:rotate(270deg);"
  rem' = pack (show (fromIntegral _iconConfig_size / 16 :: Double))


elSvg
  :: (DomBuilder t m, PostBuild t m)
  => Text
  -> Dynamic t (Map Text Text)
  -> m a
  -> m a
elSvg = elDynAttrNS (Just "http://www.w3.org/2000/svg")

svg :: (PostBuild t m, DomBuilder t m) => m a -> m a
svg = elSvg
  "svg"
  (constDyn
    (mconcat
      [ "viewBox" =: "0 0 36 36"
      , "xmlns" =: "http://www.w3.org/2000/svg"
      , "area-hidden" =: "true"
      ]
    )
  )

path :: (PostBuild t m, DomBuilder t m) => Text -> m ()
path d = elSvg "path" (constDyn ("d" =: d)) blank

circle :: (PostBuild t m, DomBuilder t m) => (Double, Double) -> Double -> m ()
circle (cx, cy) r = elSvg
  "circle"
  (constDyn
    ("cx" =: pack (show cx) <> "cy" =: pack (show cy) <> "r" =: pack (show r))
  )
  blank


errorStandardIcon :: (PostBuild t m, DomBuilder t m) => m ()
errorStandardIcon = svg $ do
  circle (18, 26.06) 1.33
  path "M18,22.61a1,1,0,0,1-1-1v-12a1,1,0,1,1,2,0v12A1,1,0,0,1,18,22.61Z"
  path
    "M18,34A16,16,0,1,1,34,18,16,16,0,0,1,18,34ZM18,4A14,14,0,1,0,32,18,14,14,0,0,0,18,4Z"

successStandardIcon :: (PostBuild t m, DomBuilder t m) => m ()
successStandardIcon = svg $ do
  path
    "M18,2A16,16,0,1,0,34,18,16,16,0,0,0,18,2Zm0,30A14,14,0,1,1,32,18,14,14,0,0,1,18,32Z"
  path
    "M28,12.1a1,1,0,0,0-1.41,0L15.49,23.15l-6-6A1,1,0,0,0,8,18.53L15.49,26,28,13.52A1,1,0,0,0,28,12.1Z"

warningStandardIcon :: (PostBuild t m, DomBuilder t m) => m ()
warningStandardIcon = svg $ do
  circle (18, 26.06) 1.33
  path "M18,22.61a1,1,0,0,1-1-1v-12a1,1,0,1,1,2,0v12A1,1,0,0,1,18,22.61Z"
  path
    "M15.0620782,1.681196 C15.6298819,0.649266355 16.7109091,0.0102219396 17.885,0.0102219396 C19.0590909,0.0102219396 20.1401181,0.649266355 20.7086433,1.68252129 L34.598644,27.2425225 C35.1407746,28.2401397 35.1174345,29.4495373 34.5372161,30.4254943 C33.9569977,31.4014514 32.905671,31.9996984 31.77,32 L4.02239323,31.9997492 C2.87409009,32.0254699 1.79902843,31.4375753 1.20106335,30.4569126 C0.603098265,29.4762499 0.572777899,28.2513179 1.12207818,27.241196 L15.0620782,1.681196 Z M2.87850767,28.1977282 C2.67060966,28.5800376 2.6820975,29.0441423 2.9086557,29.4156977 C3.1352139,29.7872532 3.5425354,30.0099959 4,30 L31.7697344,30 C32.1999191,29.9998858 32.5982478,29.7732208 32.8180821,29.4034482 C33.0379164,29.0336757 33.0467595,28.5754567 32.8413567,28.1974787 L18.9538739,2.64208195 C18.7394236,2.25234436 18.3298419,2.01022194 17.885,2.01022194 C17.4406889,2.01022194 17.0315538,2.25176692 16.8168946,2.64068753 L2.87850767,28.1977282 Z"

infoStandardIcon :: (PostBuild t m, DomBuilder t m) => m ()
infoStandardIcon = svg $ do
  circle (17.97, 10.45) 1.4
  path "M21,25H19V14.1H16a1,1,0,0,0,0,2h1V25H15a1,1,0,0,0,0,2h6a1,1,0,0,0,0-2Z"
  path
    "M18,34A16,16,0,1,1,34,18,16,16,0,0,1,18,34ZM18,4A14,14,0,1,0,32,18,14,14,0,0,0,18,4Z"

statusStandardIcon :: (PostBuild t m, DomBuilder t m) => Status -> m ()
statusStandardIcon = \case
  Danger  -> errorStandardIcon
  Warning -> warningStandardIcon
  Success -> successStandardIcon
  Info    -> infoStandardIcon

checkCircleIcon :: (PostBuild t m, DomBuilder t m) => m ()
checkCircleIcon = svg $ do
  path
    "M18,6A12,12,0,1,0,30,18,12,12,0,0,0,18,6Zm0,22A10,10,0,1,1,28,18,10,10,0,0,1,18,28Z"
  path
    "M16.34,23.74l-5-5a1,1,0,0,1,1.41-1.41l3.59,3.59,6.78-6.78a1,1,0,0,1,1.41,1.41Z"

exclamationCircleIcon :: (PostBuild t m, DomBuilder t m) => m ()
exclamationCircleIcon = svg $ do
  path
    "M18,6A12,12,0,1,0,30,18,12,12,0,0,0,18,6Zm0,22A10,10,0,1,1,28,18,10,10,0,0,1,18,28Z"
  path
    "M18,20.07a1.3,1.3,0,0,1-1.3-1.3v-6a1.3,1.3,0,1,1,2.6,0v6A1.3,1.3,0,0,1,18,20.07Z"
  circle (17.95, 23.02) 1.5

angleIcon :: (PostBuild t m, DomBuilder t m) => m ()
angleIcon = svg $ do
  path
    "M29.52,22.52,18,10.6,6.48,22.52a1.7,1.7,0,0,0,2.45,2.36L18,15.49l9.08,9.39a1.7,1.7,0,0,0,2.45-2.36Z"