{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
   Clarity Design System Icons
-}
module Components.Icon
  ( Status(..)
  , statusColor
  , Direction(..)
  , IconConfig(..)
  , icon
  , ferpIcon
  -- * Core icons
  , angleIcon
  , barsIcon
  , checkCircleIcon
  , cogIcon
  , ellipsisHorizontalIcon
  , ellipsisVerticalIcon
  , errorStandardIcon
  , exclamationCircleIcon
  , checkIcon
  , infoStandardIcon
  , searchIcon
  , statusStandardIcon
  , successStandardIcon
  , timesIcon
  , userIcon
  , warningStandardIcon
  , folderIcon
  , eyeIcon
  , eyeHideIcon
  , clockIcon
  , calendarIcon
  , circleIcon
  , dotCircleIcon
  , fileIcon
  , arrowIcon
  , filterGridIcon
  , filterGridCircleIcon
  , stepForwardIcon
  , angleDoubleIcon
  , viewColumnsIcon
  , undoIcon
  , redoIcon
  , plusIcon
  , trashIcon
  , floppyIcon
  , refreshIcon
  , pencilIcon
  , equalsIcon
  , doesNotEqualIcon
  , greaterThanOrEqIcon
  , lessThanOrEqIcon
  , doesNotStartWithIcon
  , doesNotEndWithIcon
  , doesNotContainIcon
  , startsWithIcon
  , endsWithIcon
  , containsIcon
  ) where

import           Data.Default
import           Data.Map                       ( Map )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Reflex.Dom              hiding ( value )

import           Components.Class
import           Nordtheme

data Direction = DirUp  -- ^ No rotation
               | DirRight -- ^ 90 degrees rotation (right angle :p)
               | DirDown -- ^ 180 degrees rotation
               | DirLeft -- ^ 270 degrees rotation
               deriving (Eq, Enum, Bounded)

instance Default Direction where
  def = DirUp

data IconConfig t = IconConfig
  { _iconConfig_size      :: Double
  , _iconConfig_status    :: Dynamic t (Maybe Status)
  , _iconConfig_direction :: Dynamic t Direction
  , _iconConfig_class     :: Dynamic t (Maybe Text)
  }

instance Reflex t => Default (IconConfig t) where
  def = IconConfig { _iconConfig_size      = 1
                   , _iconConfig_status    = def
                   , _iconConfig_direction = def
                   , _iconConfig_class     = def
                   }

icon :: (PostBuild t m, DomBuilder t m) => IconConfig t -> m a -> m a
icon IconConfig {..} = elDynAttr
  "div"
  (   (\s d x -> "class" =: ("icon" <> classStr x) <> "style" =: style s d)
  <$> _iconConfig_status
  <*> _iconConfig_direction
  <*> _iconConfig_class
  )
 where
  classStr = maybe "" (" " <>)
  style status direction =
    "display:inline-block;width:"
      <> rem'
      <> "rem;height:"
      <> rem'
      <> "rem;min-width:"
      <> rem'
      <> "rem;min-height:"
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
  rem' = pack (show _iconConfig_size)


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
path = path' mempty

path' :: (PostBuild t m, DomBuilder t m) => Map Text Text -> Text -> m ()
path' attrs d = elSvg "path" (constDyn ("d" =: d <> attrs)) blank

circle :: (PostBuild t m, DomBuilder t m) => (Double, Double) -> Double -> m ()
circle (cx, cy) r = elSvg
  "circle"
  (constDyn
    ("cx" =: pack (show cx) <> "cy" =: pack (show cy) <> "r" =: pack (show r))
  )
  blank

svgText :: (PostBuild t m, DomBuilder t m) => Double -> Double -> Text -> m ()
svgText x y t = elSvg
  "text"
  (constDyn ("x" =: pack (show x) <> "y" =: pack (show y)))
  (text t)

rect
  :: (PostBuild t m, DomBuilder t m)
  => Double
  -> Double
  -> Double
  -> Double
  -> m ()
rect x y width height = elSvg
  "rect"
  (constDyn
    (  "x"
    =: pack (show x)
    <> "y"
    =: pack (show y)
    <> "width"
    =: pack (show width)
    <> "height"
    =: pack (show height)
    )
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

checkIcon :: (PostBuild t m, DomBuilder t m) => m ()
checkIcon =
  svg
    $ path
        "M13.72,27.69,3.29,17.27a1,1,0,0,1,1.41-1.41l9,9L31.29,7.29a1,1,0,0,1,1.41,1.41Z"

angleIcon :: (PostBuild t m, DomBuilder t m) => m ()
angleIcon =
  svg
    $ path
        "M29.52,22.52,18,10.6,6.48,22.52a1.7,1.7,0,0,0,2.45,2.36L18,15.49l9.08,9.39a1.7,1.7,0,0,0,2.45-2.36Z"


ferpIcon :: (PostBuild t m, DomBuilder t m) => m ()
ferpIcon = svg $ do
  path'
    ("style" =: "fill:#8fbcbb")
    "M 28.985715,4.9715795 18.579006,0.1684825 a 0.8005161,0.8005161 0 0 0 -0.672435,0 L 7.4998623,4.9715795 A 0.8005161,0.8005161 0 0 0 7.035563,5.7000492 V 19.308823 a 0.8005161,0.8005161 0 0 0 0.4642993,0.72847 l 10.4067087,4.803096 a 0.8005161,0.8005161 0 0 0 0.672435,0 l 10.406709,-4.803096 a 0.8005161,0.8005161 0 0 0 0.464299,-0.72847 V 5.7000492 A 0.8005161,0.8005161 0 0 0 28.985715,4.9715795 Z M 18.242788,1.77752 26.736265,5.7000492 18.242788,9.622578 9.7493126,5.7000492 Z M 8.6365952,6.9488541 17.442273,11.015476 V 22.863115 L 8.6365952,18.796493 Z M 19.043304,22.863115 V 11.015476 L 27.848982,6.9488541 V 18.796493 Z"
  path'
    ("style" =: "fill:none;stroke:#8fbcbb;stroke-width:1.5")
    "m 18.708098,27.224358 c -4.32253,0.0073 -8.585215,1.010805 -12.457031,2.932618 -0.9567525,0.475144 -1.5617363,1.45129 -1.5615233,2.519531 v 2.069824 H 32.826262 l -0.0015,-2.069824 c -6.3e-5,-1.068568 -0.605666,-2.044798 -1.562988,-2.519531 -3.901068,-1.936334 -8.198523,-2.940243 -12.553711,-2.932618 z"

barsIcon :: (PostBuild t m, DomBuilder t m) => m ()
barsIcon = svg $ do
  path "M32,29H4a1,1,0,0,1,0-2H32a1,1,0,0,1,0,2Z"
  path "M32,19H4a1,1,0,0,1,0-2H32a1,1,0,0,1,0,2Z"
  path "M32,9H4A1,1,0,0,1,4,7H32a1,1,0,0,1,0,2Z"

cogIcon :: (PostBuild t m, DomBuilder t m) => m ()
cogIcon = svg $ do
  path
    "M18.1,11c-3.9,0-7,3.1-7,7s3.1,7,7,7c3.9,0,7-3.1,7-7S22,11,18.1,11z M18.1,23c-2.8,0-5-2.2-5-5s2.2-5,5-5c2.8,0,5,2.2,5,5S20.9,23,18.1,23z"
  path
    "M32.8,14.7L30,13.8l-0.6-1.5l1.4-2.6c0.3-0.6,0.2-1.4-0.3-1.9l-2.4-2.4c-0.5-0.5-1.3-0.6-1.9-0.3l-2.6,1.4l-1.5-0.6l-0.9-2.8C21,2.5,20.4,2,19.7,2h-3.4c-0.7,0-1.3,0.5-1.4,1.2L14,6c-0.6,0.1-1.1,0.3-1.6,0.6L9.8,5.2C9.2,4.9,8.4,5,7.9,5.5L5.5,7.9C5,8.4,4.9,9.2,5.2,9.8l1.3,2.5c-0.2,0.5-0.4,1.1-0.6,1.6l-2.8,0.9C2.5,15,2,15.6,2,16.3v3.4c0,0.7,0.5,1.3,1.2,1.5L6,22.1l0.6,1.5l-1.4,2.6c-0.3,0.6-0.2,1.4,0.3,1.9l2.4,2.4c0.5,0.5,1.3,0.6,1.9,0.3l2.6-1.4l1.5,0.6l0.9,2.9c0.2,0.6,0.8,1.1,1.5,1.1h3.4c0.7,0,1.3-0.5,1.5-1.1l0.9-2.9l1.5-0.6l2.6,1.4c0.6,0.3,1.4,0.2,1.9-0.3l2.4-2.4c0.5-0.5,0.6-1.3,0.3-1.9l-1.4-2.6l0.6-1.5l2.9-0.9c0.6-0.2,1.1-0.8,1.1-1.5v-3.4C34,15.6,33.5,14.9,32.8,14.7z M32,19.4l-3.6,1.1L28.3,21c-0.3,0.7-0.6,1.4-0.9,2.1l-0.3,0.5l1.8,3.3l-2,2l-3.3-1.8l-0.5,0.3c-0.7,0.4-1.4,0.7-2.1,0.9l-0.5,0.1L19.4,32h-2.8l-1.1-3.6L15,28.3c-0.7-0.3-1.4-0.6-2.1-0.9l-0.5-0.3l-3.3,1.8l-2-2l1.8-3.3l-0.3-0.5c-0.4-0.7-0.7-1.4-0.9-2.1l-0.1-0.5L4,19.4v-2.8l3.4-1l0.2-0.5c0.2-0.8,0.5-1.5,0.9-2.2l0.3-0.5L7.1,9.1l2-2l3.2,1.8l0.5-0.3c0.7-0.4,1.4-0.7,2.2-0.9l0.5-0.2L16.6,4h2.8l1.1,3.5L21,7.7c0.7,0.2,1.4,0.5,2.1,0.9l0.5,0.3l3.3-1.8l2,2l-1.8,3.3l0.3,0.5c0.4,0.7,0.7,1.4,0.9,2.1l0.1,0.5l3.6,1.1V19.4z"

ellipsisVerticalIcon :: (PostBuild t m, DomBuilder t m) => m ()
ellipsisVerticalIcon = svg $ do
  circle (18, 4.9)  2.9
  circle (18, 18)   2.9
  circle (18, 31.1) 2.9

ellipsisHorizontalIcon :: (PostBuild t m, DomBuilder t m) => m ()
ellipsisHorizontalIcon = svg $ do
  circle (4.9 , 18) 2.9
  circle (18  , 18) 2.9
  circle (31.1, 18) 2.9

searchIcon :: (PostBuild t m, DomBuilder t m) => m ()
searchIcon = svg $ do
  path
    "M16.33,5.05A10.95,10.95,0,1,1,5.39,16,11,11,0,0,1,16.33,5.05m0-2.05a13,13,0,1,0,13,13,13,13,0,0,0-13-13Z"
  path "M35,33.29l-7.37-7.42-1.42,1.41,7.37,7.42A1,1,0,1,0,35,33.29Z"

timesIcon :: (PostBuild t m, DomBuilder t m) => m ()
timesIcon =
  svg
    $ path
        "M19.41,18l8.29-8.29a1,1,0,0,0-1.41-1.41L18,16.59,9.71,8.29A1,1,0,0,0,8.29,9.71L16.59,18,8.29,26.29a1,1,0,1,0,1.41,1.41L18,19.41l8.29,8.29a1,1,0,0,0,1.41-1.41Z"

userIcon :: (PostBuild t m, DomBuilder t m) => m ()
userIcon = svg $ do
  path
    "M18,17a7,7,0,1,0-7-7A7,7,0,0,0,18,17ZM18,5a5,5,0,1,1-5,5A5,5,0,0,1,18,5Z"
  path
    "M30.47,24.37a17.16,17.16,0,0,0-24.93,0A2,2,0,0,0,5,25.74V31a2,2,0,0,0,2,2H29a2,2,0,0,0,2-2V25.74A2,2,0,0,0,30.47,24.37ZM29,31H7V25.73a15.17,15.17,0,0,1,22,0h0Z"

folderIcon :: (PostBuild t m, DomBuilder t m) => m ()
folderIcon =
  svg
    $ path
        "M30,9H16.42L14.11,5.82A2,2,0,0,0,12.49,5H6A2,2,0,0,0,4,7V29a2,2,0,0,0,2,2H30a2,2,0,0,0,2-2V11A2,2,0,0,0,30,9Zm0,20H6V13h7.31a2,2,0,0,0,2-2H6V7h6.49l2.61,3.59a1,1,0,0,0,.81.41H30Z"

eyeIcon :: (PostBuild t m, DomBuilder t m) => m ()
eyeIcon = svg $ do
  path
    "M33.62,17.53c-3.37-6.23-9.28-10-15.82-10S5.34,11.3,2,17.53L1.72,18l.26.48c3.37,6.23,9.28,10,15.82,10s12.46-3.72,15.82-10l.26-.48ZM17.8,26.43C12.17,26.43,7,23.29,4,18c3-5.29,8.17-8.43,13.8-8.43S28.54,12.72,31.59,18C28.54,23.29,23.42,26.43,17.8,26.43Z"
  path
    "M18.09,11.17A6.86,6.86,0,1,0,25,18,6.86,6.86,0,0,0,18.09,11.17Zm0,11.72A4.86,4.86,0,1,1,23,18,4.87,4.87,0,0,1,18.09,22.89Z"

eyeHideIcon :: (PostBuild t m, DomBuilder t m) => m ()
eyeHideIcon = svg $ do
  path
    "M25.19,20.4A6.78,6.78,0,0,0,25.62,18a6.86,6.86,0,0,0-6.86-6.86,6.79,6.79,0,0,0-2.37.43L18,13.23a4.78,4.78,0,0,1,.74-.06A4.87,4.87,0,0,1,23.62,18a4.79,4.79,0,0,1-.06.74Z"
  path
    "M34.29,17.53c-3.37-6.23-9.28-10-15.82-10a16.82,16.82,0,0,0-5.24.85L14.84,10a14.78,14.78,0,0,1,3.63-.47c5.63,0,10.75,3.14,13.8,8.43a17.75,17.75,0,0,1-4.37,5.1l1.42,1.42a19.93,19.93,0,0,0,5-6l.26-.48Z"
  path
    "M4.87,5.78l4.46,4.46a19.52,19.52,0,0,0-6.69,7.29L2.38,18l.26.48c3.37,6.23,9.28,10,15.82,10a16.93,16.93,0,0,0,7.37-1.69l5,5,1.75-1.5-26-26Zm9.75,9.75,6.65,6.65a4.81,4.81,0,0,1-2.5.72A4.87,4.87,0,0,1,13.9,18,4.81,4.81,0,0,1,14.62,15.53Zm-1.45-1.45a6.85,6.85,0,0,0,9.55,9.55l1.6,1.6a14.91,14.91,0,0,1-5.86,1.2c-5.63,0-10.75-3.14-13.8-8.43a17.29,17.29,0,0,1,6.12-6.3Z"

clockIcon :: (PostBuild t m, DomBuilder t m) => m ()
clockIcon = svg $ do
  path
    "M18,2A16,16,0,1,0,34,18,16,16,0,0,0,18,2Zm0,30A14,14,0,1,1,32,18,14,14,0,0,1,18,32Z"
  path "M18.92,18.4V10.75a1,1,0,0,0-2,0v8.72l5.9,4a1,1,0,1,0,1.11-1.66Z"
  path
    "M8,17.94A9.94,9.94,0,0,1,23.41,9.59l.85-1.36a11.55,11.55,0,1,0-8.53,21L16,27.7A10,10,0,0,1,8,17.94Z"

calendarIcon :: (PostBuild t m, DomBuilder t m) => m ()
calendarIcon = svg $ do
  path
    "M32.25,6H29V8h3V30H4V8H7V6H3.75A1.78,1.78,0,0,0,2,7.81V30.19A1.78,1.78,0,0,0,3.75,32h28.5A1.78,1.78,0,0,0,34,30.19V7.81A1.78,1.78,0,0,0,32.25,6Z"
  rect 8  14 2 2
  rect 14 14 2 2
  rect 20 14 2 2
  rect 26 14 2 2
  rect 8  19 2 2
  rect 14 19 2 2
  rect 20 19 2 2
  rect 26 19 2 2
  rect 8  24 2 2
  rect 14 24 2 2
  rect 20 24 2 2
  rect 26 24 2 2
  path "M10,10a1,1,0,0,0,1-1V3A1,1,0,0,0,9,3V9A1,1,0,0,0,10,10Z"
  path "M26,10a1,1,0,0,0,1-1V3a1,1,0,0,0-2,0V9A1,1,0,0,0,26,10Z"
  rect 13 6 10 2

circleIcon :: (PostBuild t m, DomBuilder t m) => m ()
circleIcon =
  svg
    $ path
        "M18,34A16,16,0,1,1,34,18,16,16,0,0,1,18,34ZM18,4A14,14,0,1,0,32,18,14,14,0,0,0,18,4Z"

dotCircleIcon :: (PostBuild t m, DomBuilder t m) => m ()
dotCircleIcon = svg $ do
  path "M18,11a7,7,0,1,1-7,7,7,7,0,0,1,7-7"
  path
    "M18,34A16,16,0,1,1,34,18,16,16,0,0,1,18,34ZM18,4A14,14,0,1,0,32,18,14,14,0,0,0,18,4Z"

fileIcon :: (PostBuild t m, DomBuilder t m) => m ()
fileIcon =
  svg
    $ path
        "M21.89,4H7.83A1.88,1.88,0,0,0,6,5.91V30.09A1.88,1.88,0,0,0,7.83,32H28.17A1.88,1.88,0,0,0,30,30.09V11.92Zm-.3,2.49,6,5.9h-6ZM8,30V6H20v8h8V30Z"

arrowIcon :: (PostBuild t m, DomBuilder t m) => m ()
arrowIcon =
  svg
    $ path
        "M27.66,15.61,18,6,8.34,15.61A1,1,0,1,0,9.75,17L17,9.81V28.94a1,1,0,1,0,2,0V9.81L26.25,17a1,1,0,0,0,1.41-1.42Z"

filterGridIcon :: (PostBuild t m, DomBuilder t m) => m ()
filterGridIcon =
  svg
    $ path
        "M15,25.86l2,1V20.27a1,1,0,0,0-.29-.7L10.23,13H25.79l-6.47,6.57a1,1,0,0,0-.29.7L19,28l2,1V20.68L27.58,14A1.46,1.46,0,0,0,28,13V12a1,1,0,0,0-1-1H9a1,1,0,0,0-1,1v1a1.46,1.46,0,0,0,.42,1L15,20.68Z"

filterGridCircleIcon :: (PostBuild t m, DomBuilder t m) => m ()
filterGridCircleIcon = svg $ do
  path
    "M15,25.86l2,1V20.27a1,1,0,0,0-.29-.7L10.23,13H25.79l-6.47,6.57a1,1,0,0,0-.29.7L19,28l2,1V20.68L27.58,14A1.46,1.46,0,0,0,28,13V12a1,1,0,0,0-1-1H9a1,1,0,0,0-1,1v1a1.46,1.46,0,0,0,.42,1L15,20.68Z"
  path
    "M18,2A16,16,0,1,0,34,18,16,16,0,0,0,18,2Zm0,30A14,14,0,1,1,32,18,14,14,0,0,1,18,32Z"

stepForwardIcon :: (PostBuild t m, DomBuilder t m) => m ()
stepForwardIcon = svg $ do
  path
    "M7.08,6.52a1.68,1.68,0,0,0,0,2.4L16.51,18,7.12,27.08a1.7,1.7,0,0,0,2.36,2.44h0L21.4,18,9.48,6.47A1.69,1.69,0,0,0,7.08,6.52Z"
  path
    "M26.49,5a1.7,1.7,0,0,0-1.7,1.7V29.3a1.7,1.7,0,0,0,3.4,0V6.7A1.7,1.7,0,0,0,26.49,5Z"

angleDoubleIcon :: (PostBuild t m, DomBuilder t m) => m ()
angleDoubleIcon = svg $ do
  path
    "M29,19.41a1,1,0,0,1-.71-.29L18,8.83,7.71,19.12a1,1,0,0,1-1.41-1.41L18,6,29.71,17.71A1,1,0,0,1,29,19.41Z"
  path
    "M29,30.41a1,1,0,0,1-.71-.29L18,19.83,7.71,30.12a1,1,0,0,1-1.41-1.41L18,17,29.71,28.71A1,1,0,0,1,29,30.41Z"

viewColumnsIcon :: (PostBuild t m, DomBuilder t m) => m ()
viewColumnsIcon =
  svg
    $ path
        "M31,5H5A2,2,0,0,0,3,7V29a2,2,0,0,0,2,2H31a2,2,0,0,0,2-2V7A2,2,0,0,0,31,5ZM13,29H5V7h8Zm10,0H15V7h8Z"

floppyIcon :: (PostBuild t m, DomBuilder t m) => m ()
floppyIcon =
  svg
    $ path
        "M27.36,4H6A2,2,0,0,0,4,6V30a2,2,0,0,0,2,2H30a2,2,0,0,0,2-2V8.78ZM25,30H11V22H25Zm5,0H27V22a2,2,0,0,0-2-2H11a2,2,0,0,0-2,2v8H6V6h4v6a2,2,0,0,0,2,2H24a2,2,0,0,0,2-2H12V6H26.51L30,9.59Z"

refreshIcon :: (PostBuild t m, DomBuilder t m) => m ()
refreshIcon =
  svg
    $ path
        "M22.4,11.65a1.09,1.09,0,0,0,1.09,1.09H34.43V1.81a1.09,1.09,0,1,0-2.19,0V8.95a16.41,16.41,0,1,0,1.47,15.86,1.12,1.12,0,0,0-2.05-.9,14.18,14.18,0,1,1-1.05-13.36H23.5A1.09,1.09,0,0,0,22.4,11.65Z"

undoIcon :: (PostBuild t m, DomBuilder t m) => m ()
undoIcon =
  svg
    $ path
        "M20.87,11.14h-13l5.56-5.49A1,1,0,0,0,12,4.22L4,12.13,12,20a1,1,0,0,0,1.41-1.42L7.86,13.14h13a9.08,9.08,0,0,1,9.13,9,9,9,0,0,1-5,8A1,1,0,0,0,25.93,32a11,11,0,0,0-5.06-20.82Z"

redoIcon :: (PostBuild t m, DomBuilder t m) => m ()
redoIcon =
  svg
    $ path
        "M24,4.22a1,1,0,0,0-1.41,1.42l5.56,5.49h-13A11,11,0,0,0,10.07,32,1,1,0,0,0,11,30.18a9,9,0,0,1-5-8,9.08,9.08,0,0,1,9.13-9h13l-5.54,5.48A1,1,0,0,0,24,20l8-7.91Z"

plusIcon :: (PostBuild t m, DomBuilder t m) => m ()
plusIcon =
  svg
    $ path
        "M30,17H19V6a1,1,0,1,0-2,0V17H6a1,1,0,0,0-1,1,.91.91,0,0,0,1,.94H17V30a1,1,0,1,0,2,0V19H30a1,1,0,0,0,1-1A1,1,0,0,0,30,17Z"

trashIcon :: (PostBuild t m, DomBuilder t m) => m ()
trashIcon = svg $ do
  path
    "M27.14,34H8.86A2.93,2.93,0,0,1,6,31V11.23H8V31a.93.93,0,0,0,.86,1H27.14A.93.93,0,0,0,28,31V11.23h2V31A2.93,2.93,0,0,1,27.14,34Z"
  path "M30.78,9H5A1,1,0,0,1,5,7H30.78a1,1,0,0,1,0,2Z"
  rect 21 13 2 15
  rect 13 13 2 15
  path "M23,5.86H21.1V4H14.9V5.86H13V4a2,2,0,0,1,1.9-2h6.2A2,2,0,0,1,23,4Z"

pencilIcon :: (PostBuild t m, DomBuilder t m) => m ()
pencilIcon =
  svg
    $ path
        "M33.87,8.32,28,2.42a2.07,2.07,0,0,0-2.92,0L4.27,23.2l-1.9,8.2a2.06,2.06,0,0,0,2,2.5,2.14,2.14,0,0,0,.43,0L13.09,32,33.87,11.24A2.07,2.07,0,0,0,33.87,8.32ZM12.09,30.2,4.32,31.83l1.77-7.62L21.66,8.7l6,6ZM29,13.25l-6-6,3.48-3.46,5.9,6Z"

equalsIcon :: (PostBuild t m, DomBuilder t m) => m ()
equalsIcon = svg $ do
  path "M32,23H4a1,1,0,0,1,0-2H32a1,1,0,0,1,0,2Z"
  path "M32,13H4a1,1,0,0,1,0-2H32a1,1,0,0,1,0,2Z"

doesNotEqualIcon :: (PostBuild t m, DomBuilder t m) => m ()
doesNotEqualIcon = svg $ do
  path "M32,23H4a1,1,0,0,1,0-2H32a1,1,0,0,1,0,2Z"
  path "M32,13H4a1,1,0,0,1,0-2H32a1,1,0,0,1,0,2Z"
  path "M12,31,12,31a1,1,0,0,1-2-1L24,5a1,1,0,0,1,2,1Z"

greaterThanOrEqIcon :: (PostBuild t m, DomBuilder t m) => m ()
greaterThanOrEqIcon = svg $ do
  path
    "M13.48,25.52,25.4,14,13.48,2.48A1.7,1.7,0,0,0,11.12,4.93L20.51,14,11.12,23.08A1.7,1.7,0,0,0,13.48,25.53Z"
  path
    "M27.48,19.52,15.68,31.32A1,1,0,0,1,14.27,29.91L26.07,18.11A1,1,0,0,1,27.48,19.52Z"

lessThanOrEqIcon :: (PostBuild t m, DomBuilder t m) => m ()
lessThanOrEqIcon = svg $ do
  path
    "M22.52,25.52,10.6,14,22.52,2.48A1.7,1.7,0,0,1,24.88,4.93L15.49,14,24.88,23.08A1.7,1.7,0,0,1,22.52,25.53Z"
  path
    "M8.52,19.52,20.32,31.32A1,1,0,0,0,21.73,29.91L9.93,18.11A1,1,0,0,0,8.52,19.52Z"

doesNotStartWithIcon :: (PostBuild t m, DomBuilder t m) => m ()
doesNotStartWithIcon = svg $ do
  svgText 6 22 "a__"
  path "M6,27.75,6,27.75A.75.75,0,0,1,4.5,27L15,8.25A.75.75,0,0,1,16.5,9Z"

doesNotEndWithIcon :: (PostBuild t m, DomBuilder t m) => m ()
doesNotEndWithIcon = svg $ do
  svgText 6 22 "__c"
  path "M21,27.75,21,27.75A.75.75,0,0,1,19.5,27L30,8.25A.75.75,0,0,1,31.5,9Z"

doesNotContainIcon :: (PostBuild t m, DomBuilder t m) => m ()
doesNotContainIcon = svg $ do
  svgText 6 22 "_b_"
  path "M13.5,27.75,13.5,27.75A.75.75,0,0,1,12,27L22.5,8.25A.75.75,0,0,1,24,9Z"

startsWithIcon :: (PostBuild t m, DomBuilder t m) => m ()
startsWithIcon = svg $ svgText 6 22 "a__"

endsWithIcon :: (PostBuild t m, DomBuilder t m) => m ()
endsWithIcon = svg $ svgText 6 22 "__c"

containsIcon :: (PostBuild t m, DomBuilder t m) => m ()
containsIcon = svg $ svgText 6 22 "_b_"

