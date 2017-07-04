module View.CSS where

import Prelude (discard, ($), (#))
import Data.NonEmpty (singleton)
import Text.Smolder.Markup (Attribute)
import Pux.DOM.HTML.Attributes (style)
import CSS.Background (backgroundColor)
import CSS.Color (rgb, red, lightgrey, darkgrey)
import CSS.String (fromString)
import CSS.Display (display, displayNone, block, inlineBlock, float, floatLeft)
import CSS.Font (GenericFontFamily(..), color, fontSize, fontFamily)
import CSS.Geometry (width, padding, margin)
import CSS.Size (px, em)
import CSS.TextAlign (textAlign, leftTextAlign, center)

monospace :: GenericFontFamily
monospace = GenericFontFamily $ fromString "monospace"

taStyle :: Attribute
taStyle =
    style do
      padding (px 10.0) (px 0.0) (px 10.0) (px 0.0)
      fontSize (em 1.5)
      fontFamily [ "Times New Roman" ] (singleton monospace)
      backgroundColor (rgb 243 246 198)
      textAlign leftTextAlign
      margin (px 0.0) (px 2.0) (px 0.0) (px 2.0)
      display block

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

leftPaneStyle :: Attribute
leftPaneStyle =
  style do
    width (px 420.0)
    float floatLeft

rightPaneStyle :: Attribute
rightPaneStyle =
  style do
    float floatLeft

labelAlignmentStyle :: Attribute
labelAlignmentStyle =
  style do
    display inlineBlock
    width (px 165.0)

leftPanelComponentStyle :: Attribute
leftPanelComponentStyle =
    style $ do
      margin (10.0 # px) (px 0.0) (px 20.0) (px 40.0)
      fontSize (em 1.2)

inputStyle :: Attribute
inputStyle =
  style $ do
    display displayNone

inputLabelStyle :: Attribute
inputLabelStyle =
  style do
    margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
    fontSize (em 1.1)
    display inlineBlock
    padding (px 1.0) (px 8.0) (px 0.0) (px 8.0)

errorHighlightStyle :: Attribute
errorHighlightStyle =
  style do
    color red

buttonStyle :: Boolean -> Attribute
buttonStyle enabled =
  if enabled then
    style do
      margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
      fontSize (em 1.0)
  else
    style do
      margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
      fontSize (em 1.0)
      backgroundColor lightgrey
      color darkgrey

sliderStyle :: Attribute
sliderStyle =
  style do
    width (px 160.0)
    margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)

selectionStyle :: Attribute
selectionStyle =
  style do
    margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
    fontSize (em 1.0)

canvasStyle :: Attribute
canvasStyle =
  style do
    float floatLeft
    margin (0.0 # px) (px 0.0) (px 0.0) (px 40.0)
