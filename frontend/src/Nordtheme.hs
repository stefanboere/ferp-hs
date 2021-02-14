{-# LANGUAGE OverloadedStrings #-}
{-|
  An arctic, north-bluish color palette.
  Created for the clean- and minimal flat design pattern to achieve a optimal focus and readability for code syntax
  highlighting and UI.
  It consists of a total of sixteen, carefully selected, dimmed pastel colors for a eye-comfortable, but yet colorful
  ambiance.
  Styleguide Nord
-}
module Nordtheme where

import           Clay                           ( Color
                                                , parse
                                                )
import           Data.Text

-- | Base component color of "Polar Night".
-- Used for texts, backgrounds, carets and structuring characters like curly- and square brackets.
-- Markup:
-- <div style="background-color:#2e3440; width=60; height=60"></div>
-- Styleguide Nord - Polar Night
-- rgb(46, 52, 64)
nord0 :: Text
nord0 = "#2e3440"

nord0' :: Color
nord0' = parse nord0

-- | Lighter shade color of the base component color.
-- Used as a lighter background color for UI elements like status bars.
-- Markup:
-- <div style="background-color:#3b4252; width=60; height=60"></div>
-- Styleguide Nord - Polar Night
-- rgb(59, 66, 82)
nord1 :: Text
nord1 = "#3b4252"

nord1' :: Color
nord1' = parse nord1

-- | Lighter shade color of the base component color.
-- Used as line highlighting in the editor.
-- In the UI scope it may be used as selection- and highlight color.
-- Markup:
-- <div style="background-color:#434c5e; width=60; height=60"></div>
-- Styleguide Nord - Polar Night
-- rgb(67, 76, 94)
nord2 :: Text
nord2 = "#434c5e"

nord2' :: Color
nord2' = parse nord2

-- | Lighter shade color of the base component color.
-- Used for comments, invisibles, indent- and wrap guide marker.
-- In the UI scope used as pseudoclass color for disabled elements.
-- Markup:
-- <div style="background-color:#4c566a; width=60; height=60"></div>
-- Styleguide Nord - Polar Night
-- rgb(76, 86, 106)
nord3 :: Text
nord3 = "#4c566a"

nord3' :: Color
nord3' = parse nord3

-- | Base component color of "Snow Storm".
-- Main color for text, variables, constants and attributes.
-- In the UI scope used as semi-light background depending on the theme shading design.
-- Markup:
-- <div style="background-color:#d8dee9; width=60; height=60"></div>
-- Styleguide Nord - Snow Storm
-- rgb(216, 222, 233)
nord4 :: Text
nord4 = "#d8dee9"

nord4' :: Color
nord4' = parse nord4

-- | Lighter shade color of the base component color.
-- Used as a lighter background color for UI elements like status bars.
-- Used as semi-light background depending on the theme shading design.
-- Markup:
-- <div style="background-color:#e5e9f0; width=60; height=60"></div>
-- Styleguide Nord - Snow Storm
-- rgb(229, 233, 240)
nord5 :: Text
nord5 = "#e5e9f0"

nord5' :: Color
nord5' = parse nord5

-- | Lighter shade color of the base component color.
-- Used for punctuations, carets and structuring characters like curly- and square brackets.
-- In the UI scope used as background, selection- and highlight color depending on the theme shading design.
-- Markup:
-- <div style="background-color:#eceff4; width=60; height=60"></div>
-- Styleguide Nord - Snow Storm
-- rgb(236, 239, 244)
nord6 :: Text
nord6 = "#eceff4"

nord6' :: Color
nord6' = parse nord6

-- | Bluish core color.
-- Used for classes, types and documentation tags.
-- Markup:
-- <div style="background-color:#8fbcbb; width=60; height=60"></div>
-- Styleguide Nord - Frost
-- rgb(143, 188, 187)
nord7 :: Text
nord7 = "#8fbcbb"

nord7' :: Color
nord7' = parse nord7

-- | Bluish core accent color.
-- Represents the accent color of the color palette.
-- Main color for primary UI elements and methods/functions.
-- Can be used for
--   - Markup quotes
--   - Markup link URLs
-- Markup:
-- <div style="background-color:#88c0d0; width=60; height=60"></div>
-- Styleguide Nord - Frost
-- rgb(136, 192, 208)
nord8 :: Text
nord8 = "#88c0d0"

nord8' :: Color
nord8' = parse nord8

-- | Bluish core color.
-- Used for language-specific syntactic/reserved support characters and keywords, operators, tags, units and
-- punctuations like (semi)colons,commas and braces.
-- Markup:
-- <div style="background-color:#81a1c1; width=60; height=60"></div>
-- Styleguide Nord - Frost
-- rgb(129, 161, 193)
nord9 :: Text
nord9 = "#81a1c1"

nord9' :: Color
nord9' = parse nord9

-- | Bluish core color.
-- Used for markup doctypes, import/include/require statements, pre-processor statements and at-rules (`@`).
-- Markup:
-- <div style="background-color:#5e81ac; width=60; height=60"></div>
-- Styleguide Nord - Frost
-- rgb(94, 129, 172)
nord10 :: Text
nord10 = "#5e81ac"

nord10' :: Color
nord10' = parse nord10

-- | Colorful component color.
-- Used for errors, git/diff deletion and linter marker.
-- Markup:
-- <div style="background-color:#bf616a; width=60; height=60"></div>
-- Styleguide Nord - Aurora
-- rgb(191, 97, 106)
nord11 :: Text
nord11 = "#bf616a"

nord11' :: Color
nord11' = parse nord11

-- | Colorful component color.
-- Used for annotations.
-- Markup:
-- <div style="background-color:#d08770; width=60; height=60"></div>
-- Styleguide Nord - Aurora
-- rgb(208, 135, 112)
nord12 :: Text
nord12 = "#d08770"

nord12' :: Color
nord12' = parse nord12

-- | Colorful component color.
-- Used for escape characters, regular expressions and markup entities.
-- In the UI scope used for warnings and git/diff renamings.
-- Markup:
-- <div style="background-color:#ebcb8b; width=60; height=60"></div>
-- Styleguide Nord - Aurora
-- rgb(235, 203, 139)
nord13 :: Text
nord13 = "#ebcb8b"

nord13' :: Color
nord13' = parse nord13

-- | Colorful component color.
-- Main color for Texts and attribute values.
-- In the UI scope used for git/diff additions and success visualizations.
-- Markup:
-- <div style="background-color:#a3be8c; width=60; height=60"></div>
-- Styleguide Nord - Aurora
-- rgb(163, 190, 140)
nord14 :: Text
nord14 = "#a3be8c"

nord14' :: Color
nord14' = parse nord14

-- | Colorful component color.
-- Used for numbers.
-- Markup:
-- <div style="background-color:#b48ead; width=60; height=60"></div>
-- Styleguide Nord - Aurora
-- rgb(180, 142, 173)
nord15 :: Text
nord15 = "#b48ead"

nord15' :: Color
nord15' = parse nord15


-- | Lightest grey tint
-- hsl(220, 1, 98)
white0 :: Text
white0 = "#f8f9fb"

white0' :: Color
white0' = parse white0

--hsl(220, 2, 97)
white1 :: Text
white1 = "#f2f4f8"

white1' :: Color
white1' = parse white1

-- hsl(220, 24, 63)
grey0 :: Text
grey0 = "#7b88a1"

grey0' :: Color
grey0' = parse grey0

-- Nord 14 with a higher saturation and contrast
green1 :: Text
green1 = "#456629"

green1' :: Color
green1' = parse green1


