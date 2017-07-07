module Temp (perf2melody) where

import Prelude (map, (/), ($))
import Data.Euterpea.Midi.MEvent (Performance, MEvent(..))
import Data.Midi.Player.HybridPerformance (Melody, MidiPhrase)
import Audio.SoundFont (MidiNote)
import Data.Rational (toNumber) as R
import Data.Int (toNumber) as I
import Data.Array (fromFoldable, singleton)

-- | temporary quick and dirty and non-performant translation
-- |  from HSoM's Performace to the Midi Player's Melody


perf2melody :: Performance -> Melody
perf2melody p =
  singleton (perf2phrase p)

perf2phrase :: Performance -> MidiPhrase
perf2phrase p =
  fromFoldable $ map event2note p

event2note :: MEvent -> MidiNote
event2note me =
  case me of
    (MEvent e) ->
      note 0 e.ePitch (R.toNumber e.eTime) (R.toNumber e.eDur)  (I.toNumber e.eVol / 125.0)

note :: Int -> Int -> Number -> Number -> Number -> MidiNote
note channel id timeOffset duration gain =
  { channel : channel, id : id, timeOffset : timeOffset, duration : duration, gain : gain }
