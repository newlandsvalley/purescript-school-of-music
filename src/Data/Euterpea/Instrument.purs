module Data.Euterpea.Instrument
  ( InstrumentName(..)
  , InstrumentMap(..)
  , gleitzmanName
  , read
  ) where

import Prelude (class Show, class Eq, class Ord, ($), show)
import Data.Generic.Rep as G
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Data.Foldable (foldr)
import Data.String (fromCharArray, toCharArray)
import Data.Char.Unicode (isUpper, isDigit, toLower)
import Data.Array ((:), drop)
import Data.Tuple (Tuple(..))
import Data.Map (Map, fromFoldable, lookup) as Map
import Data.Maybe (Maybe)

-- | PSoM instrument names
-- | compatible with Gleitzman soundfont names
-- | https://github.com/danigb/soundfont-player/blob/master/names/fluidR3.json
data InstrumentName =
    Accordion           | AcousticBass        | AcousticGrandPiano
  | AcousticGuitarNylon | AcousticGuitarSteel | Agogo
  | AltoSax             | Applause            | Bagpipe
  | Banjo               | BaritoneSax         | Bassoon
  | BirdTweet           | BlownBottle         | BrassSection
  | BreathNoise         | BrightAcousticPiano | Celesta
  | Cello               | ChoirAahs           | ChurchOrgan
  | Clarinet            | Clavinet            | Contrabass
  | DistortionGuitar    | DrawbarOrgan        | Dulcimer
  | ElectricBassFinger  | ElectricBassPick    | ElectricGrandPiano
  | ElectricGuitarClean | ElectricGuitarJazz  | ElectricGuitarMuted
  | ElectricPiano1      | ElectricPiano2      | EnglishHorn
  | Fiddle              | Flute               | FrenchHorn
  | FretlessBass        | Fx1Rain             | Fx2Soundtrack
  | Fx3Crystal          | Fx4Atmosphere       | Fx5Brightness
  | Fx6Goblins          | Fx7Echoes           | Fx8Scifi
  | Glockenspiel        | GuitarFretNoise     | GuitarHarmonics
  | Gunshot             | Harmonica           | Harpsichord
  | Helicopter          | HonkytonkPiano      | Kalimba
  | Koto                | Lead1Square         | Lead2Sawtooth
  | Lead3Calliope       | Lead4Chiff          | Lead5Charang
  | Lead6Voice          | Lead7Fifths         | Lead8BassLead
  | Marimba             | MelodicTom          | MusicBox
  | MutedTrumpet        | Oboe                | Ocarina
  | OrchestraHit        | OrchestralHarp      | OverdrivenGuitar
  | Pad1NewAge          | Pad2Warm            | Pad3Polysynth
  | Pad4Choir           | Pad5Bowed           | Pad6Metallic
  | Pad7Halo            | Pad8Sweep           | PanFlute
  | PercussiveOrgan     | Piccolo             | PizzicatoStrings
  | Recorder            | ReedOrgan           | ReverseCymbal
  | RockOrgan           | Seashore            | Shakuhachi
  | Shamisen            | Shanai              | Sitar
  | SlapBass1           | SlapBass2           | SopranoSax
  | SteelDrums          | StringEnsemble1     | StringEnsemble2
  | SynthBass1          | SynthBass2          | SynthBrass1
  | SynthBrass2         | SynthChoir          | SynthDrum
  | SynthStrings1       | SynthStrings2       | TaikoDrum
  | TangoAccordion      | TelephoneRing       | TenorSax
  | Timpani             | TinkleBell          | TremoloStrings
  | Trombone            | Trumpet             | Tuba
  | TubularBells        | Vibraphone          | Viola
  | Violin              | VoiceOohs           | Whistle
  | Woodblock           | Xylophone


{- original HSoM names

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

-}

derive instance genericInstrumentName :: G.Generic InstrumentName _
instance eqInstrumentName :: Eq InstrumentName where
  eq x y = GEq.genericEq x y
instance ordInstrumentName :: Ord InstrumentName where
  compare x y = GOrd.genericCompare x y
instance showInstrumentName :: Show InstrumentName where
  show x = GShow.genericShow x

-- a mapping of instrument name to channel
type InstrumentMap = Map.Map String Int

-- | convert a PSoM instrument name to the format Found
-- | in the Gleitzman instrument soundfont library
-- | https://github.com/gleitz/midi-js-soundfonts
-- | for example electric_piano_1
gleitzmanName :: InstrumentName -> String
gleitzmanName inst =
  -- the leading capital induces an unwanted underscore which we must drop
  fromCharArray $ drop 1 $ foldr f [] (toCharArray $ show inst)
    where
      f :: Char -> Array Char -> Array Char
      f c acc =
        -- all capitals should invoke an underscore unless at the start
        if (isUpper c) then
          '_' : ((toLower c) : acc)
        else if (isDigit c) then
          '_' : (c : acc)
        else
         (toLower c) : acc

-- | all this rigmarole is because there's no generic deriving for Read or Wnum
-- | at the moment in purescript.  Replace this if and when it arrives.
names :: Map.Map String InstrumentName
names =
 Map.fromFoldable
   [
     Tuple (gleitzmanName Accordion ) Accordion
   , Tuple (gleitzmanName AcousticBass) AcousticBass
   , Tuple (gleitzmanName AcousticGrandPiano) AcousticGrandPiano
   , Tuple (gleitzmanName AcousticGuitarNylon) AcousticGuitarNylon
   , Tuple (gleitzmanName AcousticGuitarSteel) AcousticGuitarSteel
   , Tuple (gleitzmanName Agogo) Agogo
   , Tuple (gleitzmanName AltoSax) AltoSax
   , Tuple (gleitzmanName Applause) Applause
   , Tuple (gleitzmanName Bagpipe) Bagpipe
   , Tuple (gleitzmanName Banjo) Banjo
   , Tuple (gleitzmanName BaritoneSax) BaritoneSax
   , Tuple (gleitzmanName Bassoon) Bassoon
   , Tuple (gleitzmanName BirdTweet) BirdTweet
   , Tuple (gleitzmanName BlownBottle) BlownBottle
   , Tuple (gleitzmanName BrassSection) BrassSection
   , Tuple (gleitzmanName BreathNoise) BreathNoise
   , Tuple (gleitzmanName BrightAcousticPiano) BrightAcousticPiano
   , Tuple (gleitzmanName Celesta) Celesta
   , Tuple (gleitzmanName Cello) Cello
   , Tuple (gleitzmanName ChoirAahs) ChoirAahs
   , Tuple (gleitzmanName ChurchOrgan) ChurchOrgan
   , Tuple (gleitzmanName Clarinet) Clarinet
   , Tuple (gleitzmanName Clavinet) Clavinet
   , Tuple (gleitzmanName Contrabass) Contrabass
   , Tuple (gleitzmanName DistortionGuitar) DistortionGuitar
   , Tuple (gleitzmanName DrawbarOrgan) DrawbarOrgan
   , Tuple (gleitzmanName Dulcimer) Dulcimer
   , Tuple (gleitzmanName ElectricBassFinger) ElectricBassFinger
   , Tuple (gleitzmanName ElectricBassPick) ElectricBassPick
   , Tuple (gleitzmanName ElectricGrandPiano) ElectricGrandPiano
   , Tuple (gleitzmanName ElectricGuitarClean) ElectricGuitarClean
   , Tuple (gleitzmanName ElectricGuitarJazz) ElectricGuitarJazz
   , Tuple (gleitzmanName ElectricGuitarMuted) ElectricGuitarMuted
   , Tuple (gleitzmanName ElectricPiano1) ElectricPiano1
   , Tuple (gleitzmanName ElectricPiano2) ElectricPiano2
   , Tuple (gleitzmanName EnglishHorn) EnglishHorn
   , Tuple (gleitzmanName Fiddle) Fiddle
   , Tuple (gleitzmanName Flute) Flute
   , Tuple (gleitzmanName FrenchHorn) FrenchHorn
   , Tuple (gleitzmanName FretlessBass) FretlessBass
   , Tuple (gleitzmanName Fx1Rain) Fx1Rain
   , Tuple (gleitzmanName Fx2Soundtrack) Fx2Soundtrack
   , Tuple (gleitzmanName Fx3Crystal) Fx3Crystal
   , Tuple (gleitzmanName Fx4Atmosphere) Fx4Atmosphere
   , Tuple (gleitzmanName Fx5Brightness) Fx5Brightness
   , Tuple (gleitzmanName Fx6Goblins) Fx6Goblins
   , Tuple (gleitzmanName Fx7Echoes) Fx7Echoes
   , Tuple (gleitzmanName Fx8Scifi) Fx8Scifi
   , Tuple (gleitzmanName Glockenspiel) Glockenspiel
   , Tuple (gleitzmanName GuitarFretNoise) GuitarFretNoise
   , Tuple (gleitzmanName GuitarHarmonics) GuitarHarmonics
   , Tuple (gleitzmanName Gunshot) Gunshot
   , Tuple (gleitzmanName Harmonica) Harmonica
   , Tuple (gleitzmanName Harpsichord) Harpsichord
   , Tuple (gleitzmanName Helicopter) Helicopter
   , Tuple (gleitzmanName HonkytonkPiano) HonkytonkPiano
   , Tuple (gleitzmanName Kalimba) Kalimba
   , Tuple (gleitzmanName Koto) Koto
   , Tuple (gleitzmanName Lead1Square) Lead1Square
   , Tuple (gleitzmanName Lead2Sawtooth) Lead2Sawtooth
   , Tuple (gleitzmanName Lead3Calliope) Lead3Calliope
   , Tuple (gleitzmanName Lead4Chiff) Lead4Chiff
   , Tuple (gleitzmanName Lead5Charang) Lead5Charang
   , Tuple (gleitzmanName Lead6Voice) Lead6Voice
   , Tuple (gleitzmanName Lead7Fifths) Lead7Fifths
   , Tuple (gleitzmanName Lead8BassLead) Lead8BassLead
   , Tuple (gleitzmanName Marimba) Marimba
   , Tuple (gleitzmanName MelodicTom) MelodicTom
   , Tuple (gleitzmanName MusicBox) MusicBox
   , Tuple (gleitzmanName MutedTrumpet) MutedTrumpet
   , Tuple (gleitzmanName Oboe) Oboe
   , Tuple (gleitzmanName Ocarina) Ocarina
   , Tuple (gleitzmanName OrchestraHit) OrchestraHit
   , Tuple (gleitzmanName OrchestralHarp) OrchestralHarp
   , Tuple (gleitzmanName OverdrivenGuitar) OverdrivenGuitar
   , Tuple (gleitzmanName Pad1NewAge) Pad1NewAge
   , Tuple (gleitzmanName Pad2Warm) Pad2Warm
   , Tuple (gleitzmanName Pad3Polysynth) Pad3Polysynth
   , Tuple (gleitzmanName Pad4Choir) Pad4Choir
   , Tuple (gleitzmanName Pad5Bowed) Pad5Bowed
   , Tuple (gleitzmanName Pad6Metallic) Pad6Metallic
   , Tuple (gleitzmanName Pad7Halo) Pad7Halo
   , Tuple (gleitzmanName Pad8Sweep) Pad8Sweep
   , Tuple (gleitzmanName PanFlute) PanFlute
   , Tuple (gleitzmanName PercussiveOrgan) PercussiveOrgan
   , Tuple (gleitzmanName Piccolo) Piccolo
   , Tuple (gleitzmanName PizzicatoStrings) PizzicatoStrings
   , Tuple (gleitzmanName Recorder) Recorder
   , Tuple (gleitzmanName ReedOrgan) ReedOrgan
   , Tuple (gleitzmanName ReverseCymbal) ReverseCymbal
   , Tuple (gleitzmanName RockOrgan) RockOrgan
   , Tuple (gleitzmanName Seashore) Seashore
   , Tuple (gleitzmanName Shakuhachi) Shakuhachi
   , Tuple (gleitzmanName Shamisen) Shamisen
   , Tuple (gleitzmanName Shanai) Shanai
   , Tuple (gleitzmanName Sitar) Sitar
   , Tuple (gleitzmanName SlapBass1) SlapBass1
   , Tuple (gleitzmanName SlapBass2) SlapBass2
   , Tuple (gleitzmanName SopranoSax) SopranoSax
   , Tuple (gleitzmanName SteelDrums) SteelDrums
   , Tuple (gleitzmanName StringEnsemble1) StringEnsemble1
   , Tuple (gleitzmanName StringEnsemble2) StringEnsemble2
   , Tuple (gleitzmanName SynthBass1) SynthBass1
   , Tuple (gleitzmanName SynthBass2) SynthBass2
   , Tuple (gleitzmanName SynthBrass1) SynthBrass1
   , Tuple (gleitzmanName SynthBrass2) SynthBrass2
   , Tuple (gleitzmanName SynthChoir) SynthChoir
   , Tuple (gleitzmanName SynthDrum) SynthDrum
   , Tuple (gleitzmanName SynthStrings1) SynthStrings1
   , Tuple (gleitzmanName SynthStrings2) SynthStrings2
   , Tuple (gleitzmanName TaikoDrum) TaikoDrum
   , Tuple (gleitzmanName TangoAccordion) TangoAccordion
   , Tuple (gleitzmanName TelephoneRing) TelephoneRing
   , Tuple (gleitzmanName TenorSax) TenorSax
   , Tuple (gleitzmanName Timpani) Timpani
   , Tuple (gleitzmanName TinkleBell) TinkleBell
   , Tuple (gleitzmanName TremoloStrings) TremoloStrings
   , Tuple (gleitzmanName TubularBells) TubularBells
   , Tuple (gleitzmanName Vibraphone) Vibraphone
   , Tuple (gleitzmanName Viola) Viola
   , Tuple (gleitzmanName Violin) Violin
   , Tuple (gleitzmanName VoiceOohs) VoiceOohs
   , Tuple (gleitzmanName Whistle) Whistle
   , Tuple (gleitzmanName Woodblock) Woodblock
   , Tuple (gleitzmanName Xylophone) Xylophone
   ]


read :: String -> Maybe InstrumentName
read g =
  Map.lookup g names
