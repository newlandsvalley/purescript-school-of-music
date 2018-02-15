module SampleText (frereJacques) where

import Prelude ((<>))

frereJacques :: String
frereJacques =
    "\"Frere Jacques\"\r\n" <>
    "-- More examples at https://github.com/newlandsvalley/purescript-school-of-music/tree/master/editor/examples \r\n" <>
    "Let \r\n" <>
    "    ln1 = Line Note qn G 3, Note qn A 3, Note qn B 3, Note qn G 3  \r\n" <>
    "    ln2 = Line Note qn B 3, Note qn C 4, Note hn D 4 \r\n" <>
    "    ln3 = Line Note en D 4, Note en E 4, Note en D 4, Note en C 4, Note qn B 3, Note qn G 3 \r\n" <>
    "    ln4 = Line Note qn G 3, Note qn D 3, Note hn G 3 \r\n" <>
    "    rest = Rest wn \r\n" <>
    "In \r\n" <>
    "  Par \r\n" <>
    "     Instrument acoustic_bass ( Transpose -12 ( Seq ln1 ln1 ln2 ln2 ln3 ln3 ln4 ln4 )) \r\n" <>
    "     Instrument vibraphone ( Transpose 12 ( Seq rest rest ln1 ln1 ln2 ln2 ln3 ln3 ln4 ln4 )) \r\n" <>
    "     Instrument acoustic_grand_piano ( Seq rest rest rest rest ln1 ln1 ln2 ln2 ln3 ln3 ln4 ln4 )"
