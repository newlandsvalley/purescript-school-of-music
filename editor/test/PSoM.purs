module Test.PSoM (psomSuite) where

import Prelude (Unit, discard, show, (<>), ($), (<<<))
import Control.Monad.Free (Free)
import Data.List (List, head, singleton, fromFoldable)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Rational (Rational, fromInt, (%))

import Data.Monoid (mempty)
import Data.Abc.Parser (parse)
import Data.Abc.PSoM.Translation (toPSoM)
import Data.Abc.PSoM.DSL (toDSL)
import Data.Abc.PSoM
import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert

-- | These tests exercise the import of ABC to the PSoM DSL

-- | tests for very simple lines of music which can be represented
-- | within a simple variable
assertSimpleLine :: forall e. String -> (List PSMusic) -> Test e
assertSimpleLine s target =
  case (parse s) of
    Right tune ->
      let
        PSoMProgram psom = toPSoM tune
      in
        case (head psom.variables) of
          Just var ->
            Assert.equal target var
          _ ->
            failure "no variables found"

    Left err ->
      failure ("parse failed: " <> (show err))

-- | test for full PSoM program
assertPSoM :: forall e. String -> PSoMProgram -> Test e
assertPSoM s target =
  case (parse s) of
    Right tune ->
      let
        psom = toPSoM tune
      in
        Assert.equal target psom

    Left err ->
      failure ("parse failed: " <> (show err))

-- | test for generated DSL text
assertDSL :: forall e. String -> String -> Test e
assertDSL s target =
  case (parse s) of
    Right tune ->
      let
        dsl = (toDSL <<< toPSoM) tune
      in
        Assert.equal target dsl

    Left err ->
      failure ("parse failed: " <> (show err))

psomSuite :: forall t. Free (TestF t) Unit
psomSuite = do
  basicSuite
  repeatsSuite
  dslSuite

basicSuite :: forall t. Free (TestF t) Unit
basicSuite =
  suite "PSoM simple line" do
    test "rest" do
      assertSimpleLine "| z z |\r\n"
        (fromFoldable [mrest, mrest])
    test "notes" do
      assertSimpleLine "| CDE |\r\n"
        (fromFoldable [mnote "C", mnote "D", mnote "E"])
    test "sharp key notes" do
      assertSimpleLine "K: D\r\n| CDE |\r\n"
        (fromFoldable [mnote "Cs", mnote "D", mnote "E"])
    test "chord" do
      assertSimpleLine "| [CDE] |\r\n"
        (singleton $ chord $ fromFoldable [note "C", note "D", note "E"])
    test "chord with duration" do
      assertSimpleLine "| [CDE]2 |\r\n"
        (singleton $ chord $ fromFoldable [notel "C" (1 % 4), notel "D" (1 % 4) , notel "E" (1 % 4) ])
    test "triplet" do
      assertSimpleLine "| (3CDE |\r\n"
        (singleton $
          tuplet
            (3 % 2)
            (fromFoldable [Right (note "C"), Right (note "D"), Right (note "E")])
        )
    test "quadruplet" do
      assertSimpleLine "| (4CDEF |\r\n"
        (singleton $
          tuplet
            (4 % 3)
            (fromFoldable [Right (note "C"), Right (note "D"), Right (note "E"), Right (note "F")])
        )
    test "broken rhythm >" do
      assertSimpleLine "| C>D |\r\n"
        (fromFoldable [mnotel "C" (3 % 16), mnotel "D" (1 % 16)])
    test "broken rhythm >>" do
      assertSimpleLine "| C>>D |\r\n"
        (fromFoldable [mnotel "C" (7 % 32), mnotel "D" (1 % 32)])
    test "broken rhythm <" do
      assertSimpleLine "| C<D |\r\n"
        (fromFoldable [mnotel "C" (1 % 16), mnotel "D" (3 % 16)])
    test "broken rhythm <<" do
      assertSimpleLine "| C<<D |\r\n"
        (fromFoldable [mnotel "C" (1 % 32), mnotel "D" (7 % 32)])
    test "tied notes" do
      assertSimpleLine "| CD-D |\r\n"
        (fromFoldable [mnote "C", mnotel "D" (1 % 4)])
    test "doubly tied notes" do
      assertSimpleLine "| CD-D-D |\r\n"
        (fromFoldable [mnote "C", mnotel "D" (3 % 8)])
    test "tied notes across bars" do
      assertSimpleLine "| CD- | D |\r\n"
        (fromFoldable [mnote "C", mnotel "D" (1 % 4)])
    test "tie into chord" do  -- we don't support ties into chords - it's ambiguous
      assertSimpleLine "| C-[CEG] |\r\n"
        (fromFoldable [mnote "C", chord $ fromFoldable [note "C", note "E", note "G"]])
    test "tie into tuplet" do  -- neither do we support ties into tuplets
      assertSimpleLine "| C- (3CEG |\r\n"
        (fromFoldable [mnote "C", (tuplet (3 % 2) $
          fromFoldable [Right (note "C"), Right (note "E"), Right (note "G")])])
    test "change unit note length" do
      assertSimpleLine "| CD |\r\nL: 1/16\r\n| E |\r\n"
        (fromFoldable [mnote "C", mnote "D", mnotel "E" (1 % 16)])
    test "change unit note length inline" do
      assertSimpleLine "| CD | [L: 1/16] | E |\r\n"
        (fromFoldable [mnote "C", mnote "D", mnotel "E" (1 % 16)])
    test "change key" do
      assertSimpleLine "| CDE |\r\nK: D\r\n| C |\r\n"
        (fromFoldable [mnote "C", mnote "D", mnote "E", mnote "Cs"])
    test "change key inline" do
      assertSimpleLine "| CDE | [K: D] | C |\r\n"
        (fromFoldable [mnote "C", mnote "D", mnote "E", mnote "Cs"])


repeatsSuite :: forall t. Free (TestF t) Unit
repeatsSuite =
  suite "PSoM programs with repeat sections" do
    test "unrepeated" do
      assertPSoM "| C D |\r\n"
        unrepeated
    test "titled" do
      assertPSoM "T: Lillasystern\r\n| C D |\r\n"
        titled
    test "simple repeat" do
      assertPSoM "|: C D :|\r\n"
        simpleRepeat
    test "unrepeated then simple repeat" do
      assertPSoM "|C D |: E F :|\r\n"
        unrepeatedThenSimpleRepeat
    test "two simple repeats" do
      assertPSoM "|: C D :|: E F :|\r\n"
        twoSimpleRepeats
    test "simple repeat then unrepeated" do
      assertPSoM "|: C D :| E F |\r\n"
        simpleRepeatThenUnrepeated
    test "variant repeat" do
      assertPSoM ":| C |1 D :|2 E :|\r\n"
        variantRepeat
    test "two variant repeats" do
      assertPSoM "|: C |1 D :|2 E :|: F |1 G :|2 A :|\r\n"
        twoVariantRepeats
    test "variant then simple" do
      assertPSoM ":| C |1 D :|2 E :|: F :|\r\n"
        variantThenSimple

dslSuite :: forall t. Free (TestF t) Unit
dslSuite =
  suite "DSL" do
    test "notes" do
      assertDSL "| CDE |\r\n"
        ( "\"untitled\"\r\n"
         <> "Let\r\n   v0 = Line Note en C 5,Note en D 5,Note en E 5 \r\n"
         <>  "In\r\n  Seq v0\r\n"
        )
    test "chord" do
      assertDSL "| [CDE] |\r\n"
        ( "\"untitled\"\r\n"
         <> "Let\r\n   v0 = Line Chord [ Note en C 5,Note en D 5,Note en E 5 ] \r\n"
         <>  "In\r\n  Seq v0\r\n"
        )
    test "triplet" do
      assertDSL "| (3CDE |\r\n"
        ( "\"untitled\"\r\n"
         <> "Let\r\n   v0 = Line Tempo 3/2 ( Line Note en C 5,Note en D 5,Note en E 5 ) \r\n"
         <>  "In\r\n  Seq v0\r\n"
        )
    test "irregular duration" do
      assertDSL "| C3/7 |\r\n"
        ( "\"untitled\"\r\n"
         <> "Let\r\n   v0 = Line Tempo 56/3 ( Note wn C 5 ) \r\n"
         <>  "In\r\n  Seq v0\r\n"
        )
    test "variant repeat" do
      assertDSL ":| C |1 D :|2 E :|\r\n"
        ( "\"untitled\"\r\n"
         <> "Let\r\n   v0 = Line Note en C 5\r\n"
         <>       "    v1 = Line Note en D 5\r\n"
         <>       "    v2 = Line Note en E 5 \r\n"
         <> "In\r\n  Seq v0 v1 v0 v2\r\n"
        )
    test "explicit tempo" do
      assertDSL "Q:1/4=120\r\n| CDE |\r\n"
        ( "\"untitled\"\r\n"
         <> "Let\r\n   v0 = Line Note en C 5,Note en D 5,Note en E 5 \r\n"
         <>  "In\r\n  Seq v0\r\n"
        )
    test "faster tempo" do
      assertDSL "Q:1/4=160\r\n| CDE |\r\n"
        ( "\"untitled\"\r\n"
         <> "Let\r\n   v0 = Line Note en C 5,Note en D 5,Note en E 5 \r\n"
         <>  "In\r\n  Tempo 4/3 (Seq v0)\r\n"
        )

-- | a basic note in octave 5, length 1/8
note :: String -> PSNote
note pc =
  PSNote { duration: 1 % 8, octave: 5, pitchClass: pc }

-- | the same note, promoted to PSMusic
mnote :: String -> PSMusic
mnote pc =
  PSNOTE $ note pc

-- | a basic note in octave 5
notel :: String -> Rational -> PSNote
notel pc duration =
  PSNote { duration: duration, octave: 5, pitchClass: pc }

-- | the same note, promoted to PSMusic
mnotel :: String -> Rational -> PSMusic
mnotel pc duration =
  PSNOTE $ notel pc duration

-- | a basic rest promoted to PSMusic
mrest :: PSMusic
mrest =
  PSREST $ PSRest { duration: 1 % 8 }

chord :: List PSNote -> PSMusic
chord notes =
  PSCHORD notes

tuplet :: Rational -> List (Either PSRest PSNote) -> PSMusic
tuplet signature notes =
  PSTUPLET (PSRestOrNoteSequence { signature : signature, notes : notes})

unrepeated :: PSoMProgram
unrepeated =
  PSoMProgram
    { variables : singleton (fromFoldable [mnote "C", mnote "D"])
    , program : fromFoldable [0]
    , tempo : fromInt 1
    , name : Nothing
    }

titled :: PSoMProgram
titled =
  PSoMProgram
    { variables : singleton (fromFoldable [mnote "C", mnote "D"])
    , program : fromFoldable [0]
    , tempo : fromInt 1
    , name : Just "Lillasystern"
    }


simpleRepeat :: PSoMProgram
simpleRepeat =
  PSoMProgram
    { variables : singleton (fromFoldable [mnote "C", mnote "D"])
    , program : fromFoldable [0, 0]
    , tempo : fromInt 1
    , name : Nothing
    }

unrepeatedThenSimpleRepeat :: PSoMProgram
unrepeatedThenSimpleRepeat =
  PSoMProgram
    { variables : fromFoldable
         [ (fromFoldable [mnote "C", mnote "D"])
         , (fromFoldable [mnote "E", mnote "F"])
         ]
    , program : fromFoldable [0, 1, 1]
    , tempo : fromInt 1
    , name : Nothing
    }

twoSimpleRepeats :: PSoMProgram
twoSimpleRepeats =
  PSoMProgram
    { variables : fromFoldable
         [ (fromFoldable [mnote "C", mnote "D"])
         , (fromFoldable [mnote "E", mnote "F"])
         ]
    , program : fromFoldable [0, 0, 1, 1]
    , tempo : fromInt 1
    , name : Nothing
    }

simpleRepeatThenUnrepeated :: PSoMProgram
simpleRepeatThenUnrepeated =
  PSoMProgram
    { variables : fromFoldable
         [ (fromFoldable [mnote "C", mnote "D"])
         , (fromFoldable [mnote "E", mnote "F"])
         ]
    , program : fromFoldable [0, 0, 1]
    , tempo : fromInt 1
    , name : Nothing
    }

variantRepeat :: PSoMProgram
variantRepeat =
  PSoMProgram
    { variables : fromFoldable
         [ singleton (mnote "C")
         , singleton (mnote "D")
         , singleton (mnote "E")
         ]
    , program : fromFoldable [0, 1, 0, 2]
    , tempo : fromInt 1
    , name : Nothing
    }

twoVariantRepeats :: PSoMProgram
twoVariantRepeats =
  PSoMProgram
    { variables : fromFoldable
         [ singleton (mnote "C")
         , singleton (mnote "D")
         , singleton (mnote "E")
         , singleton (mnote "F")
         , singleton (mnote "G")
         , singleton (mnote "A")
         ]
    , program : fromFoldable [0, 1, 0, 2, 3, 4, 3, 5]
    , tempo : fromInt 1
    , name : Nothing
    }

variantThenSimple :: PSoMProgram
variantThenSimple =
  PSoMProgram
    { variables : fromFoldable
         [ singleton (mnote "C")
         , singleton (mnote "D")
         , singleton (mnote "E")
         , singleton (mnote "F")
         ]
    , program : fromFoldable [0, 1, 0, 2, 3, 3]
    , tempo : fromInt 1
    , name : Nothing
    }
