module Test.DSL (dslSuite) where

import Prelude (Unit, discard, show, (<>))
import Control.Monad.Free (Free)

import Data.Either (Either(..))
import Data.Euterpea.DSL.Parser (PositionedParseError(..), parse)
import Data.Euterpea.Instrument (InstrumentName(..))

import Data.Euterpea.Music
import Data.Euterpea.Music1 (Music1, Note1(..))
import Data.Rational ((%))
import Data.List (List(..), (:))

import Test.Unit (Test, TestF, suite, test, failure, success)
import Test.Unit.Assert as Assert

assertParses :: forall e. String -> Test e
assertParses s =
  case parse s of
    Right music ->
      success

    Left err ->
      failure ("parse failed: " <> (show err))

assertFails :: forall e. String -> String -> Test e
assertFails s msg =
  case parse s of
    Right music ->
      failure ("parse should have failed")
    Left (PositionedParseError err) ->
      Assert.equal msg err.error

assertMusic :: forall e. String -> Music1 -> Test e
assertMusic s target =
  case parse s of
    Right music ->
      Assert.equal (show target) (show music)

    Left err ->
      failure ("parse failed: " <> (show err))

dslSuite :: forall t. Free (TestF t) Unit
dslSuite = do
  noteSuite

noteSuite :: forall t. Free (TestF t) Unit
noteSuite =
  suite "notes" do
    test "note" do
      assertMusic  "Note qn C 1 100" cq
    test "rest" do
      assertMusic  "Rest qn" rq
    test "line" do
      assertMusic  "Line Note qn C 1 100, Note qn D 1 100, Rest qn" line
    test "chord" do
      assertMusic  "Chord [ Note qn C 1 100, Note qn D 1 100 ]" chord
    test "line with chord" do
      assertMusic  "Line Note qn C 1 100, Note qn D 1 100, Chord [ Note qn C 1 100, Note qn D 1 100 ], Rest qn" lineWithChord
    test "lines" do
      assertMusic  "Seq Line Note qn C 1 100, Note qn D 1 100, Rest qn Line Note qn C 1 100, Note qn D 1 100, Rest qn" lines
    test "simple voices" do
      assertMusic "Par Seq Line Note qn C 1 100, Note qn D 1 100, Rest qn Seq Line Note qn C 1 100, Note qn D 1 100, Rest qn" simpleVoices
    test "complex voices" do
      assertMusic complexVoicesSource complexVoices
    test "repeats" do
      assertMusic repeatsSource lines
    test "round" do
      assertParses roundSource
    test "instruments" do
      assertMusic instrumentsSource instruments
    test "set marimba" do
      assertParses "Instrument marimba Note qn C 1 100"
    test "set acoustic_grand_piano" do
      assertParses "Instrument acoustic_grand_piano Note qn C 1 100"
    test "set unknown instrument" do
      assertFails "Instrument foobar Note qn C 1 100" "instrument: foobar not known"

complexVoicesSource :: String
complexVoicesSource =
   "Par " <>
     "Seq " <>
       "Line Note qn C 1 100, Note qn D 1 100, Rest qn " <>
       "Line Note qn C 1 100, Note qn D 1 100, Rest qn " <>
     "Seq " <>
       "Line Note qn C 1 100, Note qn D 1 100, Rest qn" <>
       "Line Note qn C 1 100, Note qn D 1 100, Chord [ Note qn C 1 100, Note qn D 1 100 ], Rest qn"

instrumentsSource :: String
instrumentsSource =
   "Par " <>
     "Instrument violin " <>
       "Line Note qn C 1 100, Note qn D 1 100, Rest qn " <>
     "Instrument viola " <>
       "Line Note qn C 1 100, Note qn D 1 100, Rest qn"

repeatsSource :: String
repeatsSource =
  "Let " <>
    "ln = Line Note qn C 1 100, Note qn D 1 100, Rest qn " <>
  "In " <>
    "Seq ln ln"

roundSource :: String
roundSource =
  "Let " <>
    "ln1 = Line Note qn G 3 100, Note qn A 3 100, Note qn B 3 100, Note qn G 3 100" <>
    "ln2 = Line Note qn B 3 100, Note qn C 4 100, Note hn D 4 100 " <>
    "rest = Line Rest wn" <>
  "In " <>
    "Par " <>
      "Seq ln1 ln1 ln2 ln2 " <>
      "Seq rest rest ln1 ln1 "



cq :: Music1
cq = Prim (Note (1 % 4) (Note1 (Pitch C 1) ((Volume 100) : Nil)))

dq :: Music1
dq = Prim (Note (1 % 4) (Note1 (Pitch D 1) ((Volume 100) : Nil)))

rq :: Music1
rq = Prim (Rest (1 % 4))

line :: Music1
line = Seq cq (Seq dq rq)

lineWithChord :: Music1
lineWithChord = Seq cq (Seq dq (Seq chord rq))

lines :: Music1
lines =
  Seq line line

chord :: Music1
chord = Par cq dq

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
