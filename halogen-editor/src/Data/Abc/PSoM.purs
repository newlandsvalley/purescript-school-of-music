module Data.Abc.PSoM where

import Prelude (class Show, class Eq, class Ord, (<>), (+), compare, map)
import Data.Either (Either)
import Data.List (List(..), (:), length)
import Data.Map (Map, fromFoldable)
import Data.Rational (Rational, fromInt, (%))
import Data.Tuple (Tuple(..))
import Data.Semigroup (class Semigroup)
import Data.Monoid (class Monoid)
import Data.Ordering (Ordering(..))
import Data.Generic.Rep
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

-- | Intermediate data structures involved in translating ABC to the PSoM DSL
-- | which represents a single voice in the ABC tune

data PSNote = PSNote
  { pitchClass :: String
  , octave :: Int
  , duration :: Rational
  }

derive instance genericPSNote :: Generic PSNote _
instance showPSNote :: Show PSNote where
  show = genericShow
instance eqPSNote :: Eq PSNote where
  eq = genericEq

instance ordPSNote :: Ord PSNote where
  compare (PSNote n1) (PSNote n2) =
    let
      comp1 = compare n1.octave n2.octave
    in case comp1 of
      EQ ->
        compare n1.pitchClass n2.pitchClass
      _ ->
        comp1

data PSRest = PSRest
  { duration :: Rational }

derive instance genericPSRest :: Generic PSRest _
instance showPSRest :: Show PSRest where
  show = genericShow
instance eqPPSRest :: Eq PSRest where
  eq = genericEq

instance ordPSRest :: Ord PSRest  where
  compare (PSRest {duration : d1}) (PSRest {duration : d2}) =
    compare d1 d2

data PSRestOrNoteSequence = PSRestOrNoteSequence -- for tuplets
  { signature :: Rational
  , notes :: List (Either PSRest PSNote)
  }

derive instance genericPSRestOrNoteSequence :: Generic PSRestOrNoteSequence _
instance showPSRestOrNoteSequence :: Show PSRestOrNoteSequence where
  show = genericShow
instance eqPSRestOrNoteSequence :: Eq PSRestOrNoteSequence where
  eq = genericEq

data PSMusic =
    PSNOTE PSNote
  | PSREST PSRest
  | PSCHORD (List PSNote)
  | PSTUPLET PSRestOrNoteSequence

derive instance genericPSMusic :: Generic PSMusic _
instance showPSMusic :: Show PSMusic where
  show = genericShow
instance eqPSMusic :: Eq PSMusic where
  eq = genericEq

type PSoMVariable = List PSMusic

-- | A PSoM program is represented here as a list of variables
-- | (each of which describes a short phrase of music) followed by
-- | the program proper which is simply a sequence of variable references,
-- | each indexed by the variable position.
-- | so if we make a monoid instance we have to accommodate the fact that
-- | on addition, the variable reference changes
data PSoMProgram = PSoMProgram
  { variables  :: List PSoMVariable  -- a set of PSoM variables
  , program    :: List Int           -- a sequence of variable references
  , tempo      :: Rational           -- the tune tempo (for fragments, always 1)
  }

derive instance genericPSoMProgram :: Generic PSoMProgram _
instance showPSoMProgram :: Show PSoMProgram where
  show = genericShow
instance eqPSoMProgram :: Eq PSoMProgram where
  eq = genericEq

instance semigroupPSoMProgram :: Semigroup PSoMProgram where
  append (PSoMProgram p1) (PSoMProgram p2) =
    let
      newP2Program =
        map (_ + length p1.variables) p2.program
    in
      PSoMProgram
        { variables : (p1.variables <> p2.variables)
        , program : (p1.program <> newP2Program)
        , tempo : fromInt 1
        }

instance monoidPSoMProgram :: Monoid PSoMProgram where
  mempty =
    PSoMProgram
      { variables : Nil
      , program : Nil
      , tempo : fromInt 1
      }

durationMap :: Map Rational String
durationMap =
  fromFoldable mapping

mapping :: List (Tuple Rational String)
mapping =
    Tuple (fromInt 2) "bn"
  : Tuple (fromInt 1) "wn"
  : Tuple (1 % 2) "hn"
  : Tuple (1 % 4) "qn"
  : Tuple (1 % 8) "en"
  : Tuple (1 % 16) "sn"
  : Tuple (1 % 32) "tn"
  : Tuple (1 % 64) "sfn"
  : Tuple (3 % 2) "dwn"
  : Tuple (3 % 4) "dhn"
  : Tuple (3 % 8) "dqn"
  : Tuple (3 % 16) "den"
  : Tuple (3 % 32) "dsn"
  : Tuple (7 % 8) "ddhn"
  : Tuple (7 % 16) "ddqn"
  : Tuple (7 % 32) "dden"
  : Nil
