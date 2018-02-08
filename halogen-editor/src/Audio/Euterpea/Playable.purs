module Audio.Euterpea.Playable where

import Data.Map (empty)
import Data.Euterpea.DSL.Parser (PSoM)
import Audio.Euterpea.ToMelody (perf2melody)
import Data.Euterpea.Midi.MEvent (perform1)
import Audio.SoundFont.Melody.Class (class Playable)
import Audio.SoundFont (InstrumentChannels)
import Prelude (($))

newtype PlayablePSoM = PlayablePSoM PSoM

{- we really need to have this, but not yet supported in soundfonts
instance pPSoM :: Playable PlayablePSoM where
  toMelody (PlayablePSOM { title, music } ) instrumentChannels =
    perf2melody instrumentChannels $ perform1 music
-}


instance pPSoM :: Playable PlayablePSoM where
  toMelody (PlayablePSoM { title, music } ) =
    perf2melody empty $ perform1 music
