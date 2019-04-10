module Data.Abc.PSoM.DSL (toDSL) where

import Data.Abc.PSoM

import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.List (List, length, toUnfoldable, zipWith, (..))
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Midi.Instrument (InstrumentName, gleitzmanName)
import Data.Rational (Rational, numerator, denominator, toNumber, (%))
import Prelude (map, show, (<>), ($))

-- | convert a PSoM program to the PSoM DSL
-- | (note the program represents a single voice)
toDSL :: PSoMProgram -> InstrumentName -> String
toDSL (PSoMProgram {variables, program, tempo}) instrumentName =
   (vars variables) <> (prog instrumentName tempo program)

vars :: List PSoMVariable -> String
vars vs =
  let
    f :: Int -> PSoMVariable -> String
    f index defn =
      nicelySpace [ ("v" <> show index), "=", (line defn)]
    indices = 0 .. (length vs)
    defns = zipWith f indices vs
  in
    nicelySpace ["Let\r\n", " ", (newline defns), "\r\n"]

prog :: InstrumentName -> Rational -> List Int -> String
prog instrument tempo vs =
  let
    ln =
      "Seq " <> (nicelySpace $ toUnfoldable $ map (\v -> "v" <> show v) vs)
  in
    case (toNumber tempo) of
      1.0 ->
         "In\r\n  "
            <> "  Instrument " <> (gleitzmanName instrument)
            <> (embracket ln) <> "\r\n"
      _ ->
         "In\r\n"
            <> "  Instrument " <> (gleitzmanName instrument)
            <> embracket ("Tempo " <> (fraction tempo) <> (embracket ln)) <> "\r\n"

-- wrap a string in brackets
embracket :: String -> String
embracket s = " (" <> s <> ")"

line :: List PSMusic -> String
line ms =
  "Line " <> (commaSeparate $ map music ms)

music :: PSMusic -> String
music (PSNOTE n) = note n
music (PSREST r) = rest r
music (PSCHORD c) = chord c
music (PSTUPLET t) = tuplet t

note :: PSNote -> String
note (PSNote n) =
  case (lookup n.duration durationMap) of
    Just noteLen ->
      nicelySpace ["Note", noteLen, n.pitchClass, show n.octave]
    _ ->
      nicelySpace [ "Tempo"
                  , fraction $ reciprocal n.duration
                  , "("
                  , nicelySpace ["Note", "wn", n.pitchClass, show n.octave]
                  , ")"
                  ]

rest :: PSRest -> String
rest (PSRest r) =
  case (lookup r.duration durationMap) of
    Just noteLen ->
      nicelySpace ["Rest", noteLen]
    _ ->
      nicelySpace [ "Tempo"
                  , fraction $ reciprocal r.duration
                  , "("
                  , nicelySpace ["Rest", "wn"]
                  , ")"
                  ]

chord :: List PSNote -> String
chord ns =
  nicelySpace [ "Chord", "[", commaSeparate $ map note ns, "]"]

tuplet :: PSRestOrNoteSequence -> String
tuplet (PSRestOrNoteSequence nrs) =
  nicelySpace [ "Tempo"
              , fraction nrs.signature
              , "("
              , "Line"
              , commaSeparate $ map noteOrRest nrs.notes
              , ")"
              ]

noteOrRest :: Either PSRest PSNote -> String
noteOrRest ern =
  case ern of
    Left r ->
      rest r
    Right n ->
      note n

nicelySpace :: Array String -> String
nicelySpace xs =
  intercalate " " xs

commaSeparate :: List String -> String
commaSeparate xs =
  intercalate "," xs

newline :: List String -> String
newline xs =
  intercalate "\r\n    " xs

fraction :: Rational -> String
fraction r =
  (show $ numerator r) <> "/" <> (show $ denominator r)

reciprocal :: Rational -> Rational
reciprocal r =
  (denominator r) % (numerator r)
