module Test.DSL (dslSuite) where

import Prelude (Unit, discard, show, (<>))
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Data.Euterpea.DSL.Parser (parse)
import Data.Midi.Instrument (InstrumentName(..))

import Data.Euterpea.Music
import Data.Euterpea.Music1 (Music1, Note1(..))
import Data.Rational ((%))
import Data.List (List(..), (:))

import Test.Unit (Test, TestF, suite, test, failure, success)
import Test.Unit.Assert as Assert

assertParses :: String -> Test
assertParses s =
  case parse s of
    Right psom ->
      success

    Left err ->
      failure ("parse failed: " <> (show err))

assertFails :: String -> String -> Test
assertFails s msg =
  case parse s of
    Right psom ->
      failure ("parse should have failed")
    Left {error, pos} ->
      Assert.equal msg error

assertMusic :: String -> Music1 -> Test
assertMusic s target =
  case parse s of
    Right { title, music } ->
      Assert.equal (show target) (show music)

    Left err ->
      failure ("parse failed: " <> (show err))

dslSuite :: Free TestF Unit
dslSuite = do
  noteSuite

noteSuite :: Free TestF Unit
noteSuite =
  suite "notes" do
    test "note" do
      assertMusic  "\"Test\" Note qn C 1" (cq 100)
    test "rest" do
      assertMusic  "\"Test\" Rest qn" rq
    test "line" do
      assertMusic  "\"Test\" Line Note qn C 1, Note qn D 1, Rest qn" line
    test "chord" do
      assertMusic  "\"Test\" Chord [ Note qn C 1, Note qn D 1 ]" chord
    test "line with chord" do
      assertMusic  "\"Test\" Line Note qn C 1, Note qn D 1, Chord [ Note qn C 1, Note qn D 1 ], Rest qn" lineWithChord
    test "line with control" do
      assertMusic  "\"Test\" Line Note qn C 1, Note qn D 1, Tempo 2/3 ( Line Note qn C 1, Note qn D 1, Rest qn ), Rest qn" lineWithControl
    test "lines" do
      assertMusic  "\"Test\" Seq Line Note qn C 1, Note qn D 1, Rest qn Line Note qn C 1, Note qn D 1, Rest qn" lines
    test "simple voices" do
      assertMusic "\"Test\" Par Seq Line Note qn C 1, Note qn D 1, Rest qn Seq Line Note qn C 1, Note qn D 1, Rest qn" simpleVoices
    test "complex voices" do
      assertMusic ("\"Test\"" <> complexVoicesSource) complexVoices
    test "repeats" do
      assertMusic ("\"Test\"" <> repeatsSource) lines
    test "round" do
      assertParses ("\"Test\"" <> roundSource)
    test "instruments" do
      assertMusic ("\"Test\"" <> instrumentsSource) instruments
    test "set marimba" do
      assertParses "\"Test\" Instrument marimba ( Note qn C 1 )"
    test "set acoustic_grand_piano" do
      assertParses "\"Test\" Instrument acoustic_grand_piano ( Note qn C 1 )"
    test "set unknown instrument" do
      assertFails "\"Test\" Instrument foobar ( Note qn C 1 )" "instrument: foobar not known"
    test "transpose up" do
      assertParses  "\"Test\" Transpose 12 ( Line Note qn C 1, Note qn D 1, Rest qn )"
    test "transpose down" do
      assertParses  "\"Test\" Transpose -12 ( Line Note qn C 2, Note qn D 2, Rest qn )"
    test "tempo up" do
      assertParses  "\"Test\" Tempo 3 ( Line Note qn C 2, Note qn D 2, Rest qn )"
    test "tempo down" do
      assertParses  "\"Test\" Tempo 1/2 ( Line Note qn C 1, Note qn D 1, Rest qn )"
    test "loudness up" do
      assertParses  "\"Test\" PhraseAtts Loudness 2 ( Line Note qn C 1, Note qn D 1, Rest qn )"
    test "std loudness" do
      assertParses  "\"Test\" PhraseAtts StdLoudness FFF ( Line Note qn C 1, Note qn D 1, Rest qn )"
    test "accent" do
      assertParses  "\"Test\" PhraseAtts Accent 3/2 ( Line Note qn C 1, Note qn D 1, Rest qn )"
    test "ritardando" do
      assertParses  "\"Test\" PhraseAtts Ritardando 1/2 ( Line Note qn C 1, Note qn D 1, Rest qn )"
    test "accelerando" do
      assertParses  "\"Test\" PhraseAtts Accelerando 1/2 ( Line Note qn C 1, Note qn D 1, Rest qn )"
    test "staccato" do
      assertParses  "\"Test\" PhraseAtts Staccato 1/2 ( Line Note qn C 1, Note qn D 1, Rest qn )"
    test "legato" do
      assertParses  "\"Test\" PhraseAtts Legato 1/2 ( Line Note qn C 1, Note qn D 1, Rest qn )"
    test "slurred" do
      assertParses  "\"Test\" PhraseAtts Slurred 1/2 ( Line Note qn C 1, Note qn D 1, Rest qn )"
    test "comments" do
      assertParses commentsSource

complexVoicesSource :: String
complexVoicesSource =
   "Par " <>
     "Seq " <>
       "Line Note qn C 1, Note qn D 1, Rest qn " <>
       "Line Note qn C 1, Note qn D 1, Rest qn " <>
     "Seq " <>
       "Line Note qn C 1, Note qn D 1, Rest qn" <>
       "Line Note qn C 1, Note qn D 1, Chord [ Note qn C 1, Note qn D 1 ], Rest qn"

instrumentsSource :: String
instrumentsSource =
   "Par " <>
     "Instrument violin " <>
       "( Line Note qn C 1, Note qn D 1, Rest qn )" <>
     "Instrument viola " <>
       "( Line Note qn C 1, Note qn D 1, Rest qn )"

repeatsSource :: String
repeatsSource =
  "Let " <>
    "ln = Line Note qn C 1, Note qn D 1, Rest qn " <>
  "In " <>
    "Seq ln ln"

roundSource :: String
roundSource =
  "Let " <>
    "ln1 = Line Note qn G 3, Note qn A 3, Note qn B 3, Note qn G 3" <>
    "ln2 = Line Note qn B 3, Note qn C 4, Note hn D 4 " <>
    "rest = Line Rest wn" <>
  "In " <>
    "Par " <>
      "Seq ln1 ln1 ln2 ln2 " <>
      "Seq rest rest ln1 ln1 "

commentsSource :: String
commentsSource =
   "\"Test\"\r\n" <>
   "-- comments can appear after the title \r\n" <>
   "Par" <>
     "-- and also after Par - perhaps just terminated by newline \n" <>
     "Seq" <>
       "-- and also after Seq \r\n" <>
       " Line Note qn C 1, Note qn D 1, Rest qn " <>
       "Line Note qn C 1, Note qn D 1, Rest qn " <>
     "Seq " <>
       "Line Note qn C 1, Note qn D 1, Rest qn" <>
       "Line Note qn C 1, Note qn D 1, Chord [ Note qn C 1, Note qn D 1 ], Rest qn"

cq :: Volume -> Music1
cq v = Prim (Note (1 % 4) (Note1 (Pitch C 1) ((Volume v) : Nil)))

dq :: Volume -> Music1
dq v = Prim (Note (1 % 4) (Note1 (Pitch D 1) ((Volume v) : Nil)))

rq :: Music1
rq = Prim (Rest (1 % 4))

line :: Music1
line = lineAtVol 100

lineAtVol :: Volume -> Music1
lineAtVol v = Seq (cq v) (Seq (dq v) rq)

lineWithChord :: Music1
lineWithChord = Seq (cq 100) (Seq (dq 100) (Seq chord rq))

lineWithControl :: Music1
lineWithControl = Seq (cq 100) (Seq (dq 100) (Seq (tempo line) rq))

lines :: Music1
lines =
  Seq line line

chord :: Music1
chord = Par (cq 100) (dq 100)

tempo :: Music1 -> Music1
tempo = Modify (Tempo (2 % 3))

violin :: Music1 -> Music1
violin = Modify (Instrument Violin)

viola :: Music1 -> Music1
viola = Modify (Instrument Viola)

simpleVoices :: Music1
simpleVoices = Par line line

complexVoices :: Music1
complexVoices = Par (Seq line line) (Seq line lineWithChord)

instruments :: Music1
instruments = Par (violin line) (viola line)
