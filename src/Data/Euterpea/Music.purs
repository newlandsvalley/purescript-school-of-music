module Data.Euterpea.Music where


import Prelude (class Show, class Eq, class Ord, class Bounded)
import Data.Rational (Rational)
import Data.List (List)
import Data.Generic.Rep as G
import Data.Eq.Generic as GEq
import Data.Ord.Generic as GOrd
import Data.Show.Generic as GShow
import Data.Bounded.Generic as GBounded
import Data.Euterpea.Dynamics (Dynamic)
import Data.Midi.Instrument (InstrumentName)

infixr 5 Seq as :+:
infixr 5 Par as :=:

type AbsPitch = Int
type Octave = Int
data Pitch = Pitch PitchClass Octave

derive instance genericPitch :: G.Generic Pitch _
instance eqPitch :: Eq Pitch where
  eq x y = GEq.genericEq x y
instance ordPitch :: Ord Pitch where
  compare x y = GOrd.genericCompare x y
instance showPitch :: Show Pitch where
  show x = GShow.genericShow x

type Dur   = Rational
data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
                 |  Bf | Ass | B | Bs | Bss
type Volume = Int

derive instance genericPitchClass :: G.Generic PitchClass _
instance eqPitchClass :: Eq PitchClass where
  eq x y = GEq.genericEq x y
instance ordPitchClass :: Ord PitchClass where
  compare x y = GOrd.genericCompare x y
instance showPitchClass :: Show PitchClass where
  show x = GShow.genericShow x
instance boundedPitchClass :: Bounded PitchClass where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop

data Primitive a =
    Note Dur a
  | Rest Dur

derive instance genericPrimitive :: G.Generic (Primitive a) _
instance eqPrimitive :: Eq a => Eq (Primitive a) where
  eq x y = GEq.genericEq x y
instance ordPrimitive :: Ord a => Ord (Primitive a) where
  compare x y = GOrd.genericCompare x y
instance showPrimitive :: Show a => Show (Primitive a) where
  show x = GShow.genericShow x

data Music a  =
     Prim (Primitive a)               --  primitive value
   | Seq (Music a) (Music a)          --  sequential composition
   | Par (Music a) (Music a)          --  parallel composition
   | Modify Control (Music a)         --  modifier

derive instance genericMusic :: G.Generic (Music a) _
instance eqMusic :: Eq a => Eq (Music a) where
  eq x y = GEq.genericEq x y
instance ordMusic :: Ord a => Ord (Music a) where
  compare x y = GOrd.genericCompare x y
instance showMusic :: Show a => Show (Music a) where
  show x = GShow.genericShow x


data Control =
    Tempo       Rational                 --  scale the tempo
  | Transpose   AbsPitch                 --  transposition
  | Instrument  InstrumentName           --  instrument label
  | Phrase      (List PhraseAttribute)   --  phrase attributes
  | KeySig      PitchClass Mode          --  key signature and mode
  | Custom      String                   --  for user-specified controls

derive instance genericControl :: G.Generic Control _
instance eqControl :: Eq Control where
  eq x y = GEq.genericEq x y
instance ordControl :: Ord Control where
  compare x y = GOrd.genericCompare x y
instance showControl :: Show Control where
  show x = GShow.genericShow x

data Mode =
   Major
 | Minor

derive instance genericMode :: G.Generic Mode _
instance eqMode :: Eq Mode where
  eq x y = GEq.genericEq x y
instance ordMode :: Ord Mode where
  compare x y = GOrd.genericCompare x y
instance showMode :: Show Mode where
  show x = GShow.genericShow x

data PhraseAttribute  =
    Dyn Dynamic
  | Tmp Tempo
  | Art Articulation
  | Orn Ornament

derive instance genericPhraseAttribute :: G.Generic PhraseAttribute _
instance eqPhraseAttribute :: Eq PhraseAttribute where
  eq x y = GEq.genericEq x y
instance ordPhraseAttribute :: Ord PhraseAttribute where
  compare x y = GOrd.genericCompare x y
instance showPhraseAttribute :: Show PhraseAttribute where
  show x = GShow.genericShow x

data Tempo =
    Ritardando Rational
  | Accelerando Rational

derive instance genericTempo :: G.Generic Tempo _
instance eqTempo :: Eq Tempo where
  eq x y = GEq.genericEq x y
instance ordTempo :: Ord Tempo where
  compare x y = GOrd.genericCompare x y
instance showTempo :: Show Tempo where
  show x = GShow.genericShow x

data Articulation =
    Staccato Rational | Legato Rational | Slurred Rational
 |  Tenuto | Marcato | Pedal | Fermata | FermataDown | Breath
 |  DownBow | UpBow | Harmonic | Pizzicato | LeftPizz
 |  BartokPizz | Swell | Wedge | Thumb | Stopped

derive instance genericArticulation :: G.Generic Articulation _
instance eqArticulation :: Eq Articulation where
  eq x y = GEq.genericEq x y
instance ordArticulation :: Ord Articulation where
  compare x y = GOrd.genericCompare x y
instance showArticulation :: Show Articulation where
  show x = GShow.genericShow x

data Ornament =
    Trill | Mordent | InvMordent | DoubleMordent
 |  Turn | TrilledTurn | ShortTrill
 |  Arpeggio | ArpeggioUp | ArpeggioDown
 |  Instruction String | Head NoteHead
 |  DiatonicTrans Int

derive instance genericOrnament :: G.Generic Ornament _
instance eqOrnament :: Eq Ornament where
  eq x y = GEq.genericEq x y
instance ordOrnament :: Ord Ornament where
  compare x y = GOrd.genericCompare x y
instance showOrnament :: Show Ornament where
  show x = GShow.genericShow x

data NoteHead =
    DiamondHead | SquareHead | XHead | TriangleHead
 |  TremoloHead | SlashHead | ArtHarmonic | NoHead

derive instance genericNoteHead :: G.Generic NoteHead _
instance eqNoteHead :: Eq NoteHead where
  eq x y = GEq.genericEq x y
instance ordNoteHead :: Ord NoteHead where
  compare x y = GOrd.genericCompare x y
instance showNoteHead :: Show NoteHead where
  show x = GShow.genericShow x

data NoteAttribute =
    Volume  Int   --  MIDI convention: 0=min, 127=max
 |  Fingering Int
 |  Dynamics String
 |  Params (List Number)

derive instance genericNoteAttribute :: G.Generic NoteAttribute _
instance eqNoteAttribute :: Eq NoteAttribute where
  eq x y = GEq.genericEq x y
instance ordNoteAttribute :: Ord NoteAttribute where
  compare x y = GOrd.genericCompare x y
instance showNoteAttribute :: Show NoteAttribute where
  show x = GShow.genericShow x
