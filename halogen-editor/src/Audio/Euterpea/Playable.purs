module Audio.Euterpea.Playable where

import Data.Euterpea.DSL.Parser (PSoM)
import Audio.Euterpea.ToMelody (perf2melody)
import Data.Euterpea.Midi.MEvent (perform1)
import Audio.SoundFont.Melody.Class (class Playable)
import Prelude (($))

newtype PlayablePSoM = PlayablePSoM PSoM

instance pPSoM :: Playable PlayablePSoM where
  toMelody (PlayablePSoM { title, music }) instrumentChannels =
    perf2melody instrumentChannels $ perform1 music
