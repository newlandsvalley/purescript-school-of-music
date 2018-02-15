module Halogen.PlayerComponent.Style (
    capsuleStyle
  , playerBlockStyle
  , playerStyle
  , buttonStyle
  ) where

import Prelude (discard)
import CSS.TextAlign (center, textAlign)
import CSS (color, fromString)
import CSS.Background (background, backgroundImages)
import CSS.Border (border, borderRadius, solid)
import CSS.Box (boxShadow)
import CSS.Color (rgb, rgba)
import CSS.Display (display, inlineBlock)
import CSS.Geometry (width, height, margin)
import CSS.Overflow (hidden, overflow)
import CSS.Size (px)
import Halogen (IProp)
import Halogen.HTML.CSS (style)

centreStyle :: ∀ i r. IProp (style :: String | r) i
centreStyle =
  style do
    textAlign center

-- | the capsule is the bit in the centre of the player widget that shows progress
-- | through the recording
capsuleStyle :: ∀ i r. IProp (style :: String | r) i
capsuleStyle =
  style do
    border solid (px 1.0) (rgb 0 0 0)
    margin (px 9.0) (px 0.0) (px 8.0) (px 5.0)
    -- margin (px 8.0) (px 0.0) (px 8.0) (px 0.0) -- ??
    borderRadius (px 5.0) (px 5.0) (px 5.0) (px 5.0)
    -- backgroundColor (rgb 0 0 0)
    background (rgb 0 0 0)
    backgroundImages
      [ fromString "-webkit-gradient(linear, left top, left bottom, color-stop(1, rgba(0,0,0,0.5)), color-stop(0, #333))"
      , fromString "-webkit-linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-moz-linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-ms-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-o-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      ]
    boxShadow (px 0.0) (px 0.0) (px 0.0) (rgb 5 5 5)
    overflow hidden
    display inlineBlock
    width (px 220.0)
    height (px 20.0)

-- | the basic style of the outline of the player which surrounds
-- | both the buttons and the capsule
playerBlockStyle :: ∀ i r. IProp (style :: String | r) i
playerBlockStyle =
  style do
    margin (px 10.0) (px 0.0) (px 10.0) (px 0.0)
    background (rgba 0 0 0 0.7)
    border solid (px 1.0) (rgb 0 0 0)
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)
    width (px 320.0)
    height (px 42.0)

-- the style of the player
playerStyle :: ∀ i r. IProp (style :: String | r) i
playerStyle =
  style do
    border solid (px 1.0) (rgb 0 0 0)
    backgroundImages
      [ fromString "-webkit-gradient(linear,left top,left bottom,from(rgba(66,66,66,1)),to(rgba(22,22,22,1)))"
      , fromString "-webkit-linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-moz-linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-ms-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-o-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      ]
    boxShadow (px 0.0) (px 0.0) (px 10.0) (rgb 15 15 15) -- #fff
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)
    color (rgba 255 255 255 0.8)
    -- "text-shadow", "1px 1px 2px #000"  ???

-- player button styling
buttonStyle :: ∀ i r. IProp (style :: String | r) i
buttonStyle =
  style do
    width (px 24.0)
    height (px 24.0)
    margin (px 6.0) (px 0.0) (px 1.0) (px 8.0)
