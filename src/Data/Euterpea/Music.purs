module Data.Euterpea.Music where


import Prelude (class Show, class Eq, class Ord, class Bounded, class Functor, (<>), show)
import Data.Rational (Rational)
import Data.Generic.Rep as G
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Data.Generic.Rep.Bounded as GBounded


infixr 5 MSeq as :+:
infixr 5 MPar as :=:

type AbsPitch = Int
type Octave = Int
data Pitch = Pitch PitchClass Octave
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
   | MSeq (Music a) (Music a)         --  sequential composition
   | MPar (Music a) (Music a)         --  parallel composition
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
  | Phrase      (Array PhraseAttribute)  --  phrase attributes
  | KeySig      PitchClass Mode          --  key signature and mode
  | Custom      String			             --  for user-specified controls

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

data InstrumentName =
     AcousticGrandPiano     | BrightAcousticPiano    | ElectricGrandPiano
  |  HonkyTonkPiano         | RhodesPiano            | ChorusedPiano
  |  Harpsichord            | Clavinet               | Celesta
  |  Glockenspiel           | MusicBox               | Vibraphone
  |  Marimba                | Xylophone              | TubularBells
  |  Dulcimer               | HammondOrgan           | PercussiveOrgan
  |  RockOrgan              | ChurchOrgan            | ReedOrgan
  |  Accordion              | Harmonica              | TangoAccordion
  |  AcousticGuitarNylon    | AcousticGuitarSteel    | ElectricGuitarJazz
  |  ElectricGuitarClean    | ElectricGuitarMuted    | OverdrivenGuitar
  |  DistortionGuitar       | GuitarHarmonics        | AcousticBass
  |  ElectricBassFingered   | ElectricBassPicked     | FretlessBass
  |  SlapBass1              | SlapBass2              | SynthBass1
  |  SynthBass2             | Violin                 | Viola
  |  Cello                  | Contrabass             | TremoloStrings
  |  PizzicatoStrings       | OrchestralHarp         | Timpani
  |  StringEnsemble1        | StringEnsemble2        | SynthStrings1
  |  SynthStrings2          | ChoirAahs              | VoiceOohs
  |  SynthVoice             | OrchestraHit           | Trumpet
  |  Trombone               | Tuba                   | MutedTrumpet
  |  FrenchHorn             | BrassSection           | SynthBrass1
  |  SynthBrass2            | SopranoSax             | AltoSax
  |  TenorSax               | BaritoneSax            | Oboe
  |  Bassoon                | EnglishHorn            | Clarinet
  |  Piccolo                | Flute                  | Recorder
  |  PanFlute               | BlownBottle            | Shakuhachi
  |  Whistle                | Ocarina                | Lead1Square
  |  Lead2Sawtooth          | Lead3Calliope          | Lead4Chiff
  |  Lead5Charang           | Lead6Voice             | Lead7Fifths
  |  Lead8BassLead          | Pad1NewAge             | Pad2Warm
  |  Pad3Polysynth          | Pad4Choir              | Pad5Bowed
  |  Pad6Metallic           | Pad7Halo               | Pad8Sweep
  |  FX1Train               | FX2Soundtrack          | FX3Crystal
  |  FX4Atmosphere          | FX5Brightness          | FX6Goblins
  |  FX7Echoes              | FX8SciFi               | Sitar
  |  Banjo                  | Shamisen               | Koto
  |  Kalimba                | Bagpipe                | Fiddle
  |  Shanai                 | TinkleBell             | Agogo
  |  SteelDrums             | Woodblock              | TaikoDrum
  |  MelodicDrum            | SynthDrum              | ReverseCymbal
  |  GuitarFretNoise        | BreathNoise            | Seashore
  |  BirdTweet              | TelephoneRing          | Helicopter
  |  Applause               | Gunshot                | Percussion
  |  CustomInstrument String

derive instance genericInstrumentName :: G.Generic InstrumentName _
instance eqInstrumentName :: Eq InstrumentName where
  eq x y = GEq.genericEq x y
instance ordInstrumentName :: Ord InstrumentName where
  compare x y = GOrd.genericCompare x y
instance showInstrumentName :: Show InstrumentName where
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

data Dynamic  =
    Accent Rational
  | Crescendo Rational
  | Diminuendo Rational
  | StdLoudness StdLoudness
  | Loudness Rational

derive instance genericDynamic :: G.Generic Dynamic _
instance eqDynamic :: Eq Dynamic where
  eq x y = GEq.genericEq x y
instance ordDynamic :: Ord Dynamic where
  compare x y = GOrd.genericCompare x y
instance showDynamic :: Show Dynamic where
  show x = GShow.genericShow x

data StdLoudness = PPP | PP | P | MP | SF | MF | NF | FF | FFF

derive instance genericStdLoudness :: G.Generic StdLoudness _
instance eqStdLoudness :: Eq StdLoudness where
  eq x y = GEq.genericEq x y
instance ordStdLoudness :: Ord StdLoudness where
  compare x y = GOrd.genericCompare x y
instance showStdLoudness :: Show StdLoudness where
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
 |  Params (Array Number)

derive instance genericNoteAttribute :: G.Generic NoteAttribute _
instance eqNoteAttribute :: Eq NoteAttribute where
  eq x y = GEq.genericEq x y
instance ordNoteAttribute :: Ord NoteAttribute where
  compare x y = GOrd.genericCompare x y
instance showNoteAttribute :: Show NoteAttribute where
  show x = GShow.genericShow x
