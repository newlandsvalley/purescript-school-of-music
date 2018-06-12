module Test.Performance (performanceSuite) where

import Prelude (Unit, discard, show, (<>))
import Control.Monad.Free (Free)

import Data.Either (Either(..))

import Data.Euterpea.DSL.Parser (parse)
import Data.Euterpea.Midi.MEvent (MEvent(..), Performance, perform1)
import Data.Midi.Instrument (InstrumentName(..))
import Data.Rational ((%))
import Data.List (List(..), (:))

import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert

assertPerformance :: String -> Performance -> Test
assertPerformance s target =
  case parse s of
    Right { title, music } ->
      Assert.equal target (perform1 music)

    Left err ->
      failure ("parse failed: " <> (show err))

performanceSuite :: Free TestF Unit
performanceSuite = do
  dynamicsSuite

dynamicsSuite :: Free TestF Unit
dynamicsSuite =
  suite "dynamics" do
    test "loudness" do
      assertPerformance  "\"Test\" PhraseAtts Loudness 50 ( Line Note qn C 1 )" (loudness 50)
    -- test "voices" do
    --    assertPerformance  voicesSource Nil
    -- crescendo seems to start to loud at max MIDI velocity in HSoM !
    -- test "crescendo" do
    --  assertPerformance  "PhraseAtts Crescendo 1/2 ( Line Note qn C 1 50, Note qn C 1 50, Note qn C 1 50)" loudness50
    test "diminuendo" do
        assertPerformance  "\"Test\" PhraseAtts Diminuendo 1/2 ( Line Note qn C 1, Note qn C 1, Note qn C 1)" diminuendoResult
    test "accent" do
        assertPerformance  "\"Test\" PhraseAtts Accent 1/2 ( Line Note qn C 1 )" (loudness 64)
    test "FFF" do
        assertPerformance  "\"Test\" PhraseAtts StdLoudness FFF ( Line Note qn C 1 )" (loudness 120)
    test "PPP" do
        assertPerformance  "\"Test\" PhraseAtts StdLoudness PPP ( Line Note qn C 1 )" (loudness 40)
    test "ritardando" do
        assertPerformance  "\"Test\" PhraseAtts Ritardando 1/2 ( Line Note qn C 1, Note qn C 1, Note qn C 1)" ritardandoResult
    test "accelerando" do
        assertPerformance  "\"Test\" PhraseAtts Accelerando 1/2 ( Line Note qn C 1, Note qn C 1, Note qn C 1)" accelerandoResult
    test "staccato" do
        assertPerformance  "\"Test\" PhraseAtts Staccato 1/2 ( Line Note qn C 1, Note qn C 1, Note qn C 1)" staccatoResult
    test "legato" do
        assertPerformance  "\"Test\" PhraseAtts Legato 5/4 ( Line Note qn C 1, Note qn C 1, Note qn C 1)" legatoResult
    test "slurred" do
        assertPerformance  "\"Test\" PhraseAtts Slurred 5/4 ( Line Note qn C 1, Note qn C 1, Note qn C 1)" slurredResult


loudness  :: Int -> Performance
loudness v =
  ((MEvent { eDur: 1 % 2, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 0 % 1, eVol: v }) : Nil)

diminuendoResult :: Performance
diminuendoResult =
  ((MEvent { eDur: 1 % 2, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 0 % 1, eVol: 127 }) :
   (MEvent { eDur: 1 % 2, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 1 % 2, eVol: 106 }) :
   (MEvent { eDur: 1 % 2, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 1 % 1, eVol: 85 }) :
   Nil)

ritardandoResult :: Performance
ritardandoResult =
  ((MEvent { eDur: 7 % 12, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 0 % 1, eVol: 127 }) :
   (MEvent { eDur: 3 % 4, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 7 % 12, eVol: 127 }) :
   (MEvent { eDur: 11 % 12, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 4 % 3, eVol: 127 }) :
   Nil)

accelerandoResult :: Performance
accelerandoResult =
  ((MEvent { eDur: 5 % 12, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 0 % 1, eVol: 127 }) :
   (MEvent { eDur: 1 % 4, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 5 % 12, eVol: 127 }) :
   (MEvent { eDur: 1 % 12, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 2 % 3, eVol: 127 }) :
  Nil)

staccatoResult :: Performance
staccatoResult =
  ((MEvent { eDur: 1 % 4, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 0 % 1, eVol: 127 }) :
   (MEvent { eDur: 1 % 4, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 1 % 2, eVol: 127 }) :
   (MEvent { eDur: 1 % 4, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 1 % 1, eVol: 127 }) :
  Nil)

legatoResult :: Performance
legatoResult =
  ((MEvent { eDur: 5 % 8, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 0 % 1, eVol: 127 }) :
   (MEvent { eDur: 5 % 8, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 1 % 2, eVol: 127 }) :
   (MEvent { eDur: 5 % 8, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 1 % 1, eVol: 127 }) :
  Nil)

slurredResult :: Performance
slurredResult =
  ((MEvent { eDur: 5 % 8, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 0 % 1, eVol: 127 }) :
   (MEvent { eDur: 5 % 8, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 1 % 2, eVol: 127 }) :
   (MEvent { eDur: 1 % 2, eInst: AcousticGrandPiano, eParams: Nil, ePitch: 24, eTime: 1 % 1, eVol: 127 }) :
  Nil)


voicesSource :: String
voicesSource =
  "Let \r\n" <>
  "  ln1 = Line Note qn G 3 100, Note qn A 3 100, Note qn B 3 100, Note qn G 3 100  \r\n" <>
  " In \r\n" <>
  "   Par \r\n" <>
  "     Instrument acoustic_bass ( Tempo 1/2 ( Seq ln1 )) \r\n" <>
  "     Instrument vibraphone ( PhraseAtts Loudness 50 (Seq ln1 ln1 ))\r\n" <>
  "     Instrument acoustic_grand_piano ( Seq ln1 ln1 )"
