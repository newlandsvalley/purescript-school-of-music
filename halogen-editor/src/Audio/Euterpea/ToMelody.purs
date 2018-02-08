module Audio.Euterpea.ToMelody (perf2melody) where

import Prelude (($), (+), (-), (/), (>))
import Data.Midi.Instrument (InstrumentName)
import Audio.SoundFont (InstrumentChannels, MidiNote)
import Audio.SoundFont.Melody (Melody, MidiPhrase)
import Data.Euterpea.Midi.MEvent (Performance, MEvent(..))
import Data.Rational (toNumber) as R
import Data.Int (toNumber) as I
import Data.Array (reverse, (:))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (lookup)
-- import Debug.Trace (traceShow)

-- | Convert a Performance to a Melody described by a hybrid performance
-- | where the melody is made up of short phrases, each of which will
-- | play to completion before the MIDI player is allowed to interrupt them.
-- |
-- | This is trickier with polyphonic music than with monophonic.  You cannot
-- | complete a phrase at a note where there are future notes that will ring
-- | while this one is still playing.  So, when we reach a potential phrase
-- | boundary, we save this note in pendingCutoff and wait to see what the
-- | next note brings.  If it starts before the pending note is due to end
-- | then we accumulate the cutoffNote and replace it with the new note (and so
-- | the process repeats next go).  Otherwise, the cutoff is safe and we can
-- | complete the phrase.


data Accum = Accum
  {
    current :: MidiPhrase              -- the current phrase being added to
  , pendingCutoff :: Maybe MidiNote    -- a potential phrase cutoff note (see above)
  , final :: Melody                    -- the overall melody (an array of phrases)
  , runningLength :: Number            -- the overall length of so-far completed phrases
  }

initialAccum :: Accum
initialAccum = Accum {
      current : []
    , pendingCutoff : Nothing
    , final : []
    , runningLength : 0.0
    }

getRunningLength :: Accum -> Number
getRunningLength (Accum a) = a.runningLength

getPendingCutoff :: Accum -> Maybe MidiNote
getPendingCutoff (Accum a) = a.pendingCutoff

getCurrent :: Accum -> MidiPhrase
getCurrent (Accum a) = a.current

getFinal :: Accum -> Melody
getFinal (Accum a) = a.final

phraseCutoff :: Number
phraseCutoff = 0.8

perf2melody :: InstrumentChannels -> Performance -> Melody
perf2melody ics p =
  let
    acc = perf2accum ics p
    pendingCutoff = getPendingCutoff acc
    rlength = getRunningLength acc
    final = case pendingCutoff of
      Just lastNote ->
        let
          lastNote' = lastNote { timeOffset = lastNote.timeOffset -rlength }
        in
          (reverse $ (lastNote' : getCurrent acc)) : getFinal acc
      _ ->
        (reverse $ getCurrent acc) : getFinal acc
  in
    reverse final

perf2accum :: InstrumentChannels -> Performance -> Accum
perf2accum instrumentChans p =
  foldl f initialAccum p
    where
      f:: Accum -> MEvent -> Accum
      f acc me =
        case me of
          (MEvent e) ->
             let
               rlength = getRunningLength acc
               nextNote :: MidiNote
               nextNote = event2note instrumentChans me
             in
               case getPendingCutoff acc of
                 Just lastNote ->
                   if (lastNote.timeOffset + lastNote.duration > nextNote.timeOffset) then
                      -- it's not safe to perform the cutoff , so shift the nextNote to the
                      -- pending cutoff and the pending cutoff to the current buffer
                      let
                        lastNote' = lastNote { timeOffset = lastNote.timeOffset -rlength }
                      in
                        Accum {
                          current : (lastNote' : getCurrent acc)
                        , pendingCutoff : Just nextNote
                        , final : getFinal acc
                        , runningLength : rlength
                        }
                  else
                    -- it's safe to perform the cut0ff
                    let
                       newRlength = lastNote.timeOffset + lastNote.duration
                       lastNote' = lastNote { timeOffset = lastNote.timeOffset -rlength }
                       newPhrase = reverse $ (lastNote' : getCurrent acc)
                       -- nextNote' = nextNote { timeOffset = lastNote.timeOffset - newRlength }
                       nextNote' = nextNote { timeOffset = 0.0 }
                     in
                       Accum {
                           current : [nextNote']
                         , pendingCutoff : Nothing
                         , final : (newPhrase : (getFinal acc))
                         , runningLength : nextNote.timeOffset
                         }
                 _ ->
                   -- there is no cutoff pending
                   if (nextNote.timeOffset - rlength > phraseCutoff) then
                     -- start a new pending cutoff
                     Accum {
                        current : getCurrent acc
                      , pendingCutoff : Just nextNote
                      , final : getFinal acc
                      , runningLength : rlength
                      }
                   else
                     -- just accumulate the note in the current phrase
                     let
                       nextNote' = nextNote { timeOffset = nextNote.timeOffset - rlength }
                     in
                       Accum {
                           current : (nextNote' : (getCurrent acc))
                         , pendingCutoff : Nothing
                         , final : getFinal acc
                         , runningLength : rlength
                        }

event2note :: InstrumentChannels -> MEvent -> MidiNote
event2note instrumentChans (MEvent e) =
  let
    channel = chan e.eInst instrumentChans
  in
    note channel e.ePitch (R.toNumber e.eTime) (R.toNumber e.eDur)  (I.toNumber e.eVol / 125.0)

note :: Int -> Int -> Number -> Number -> Number -> MidiNote
note channel id timeOffset duration gain =
  { channel : channel, id : id, timeOffset : timeOffset, duration : duration, gain : gain }

-- | look up the instrument in the map, defaulting to channel 0
chan :: InstrumentName -> InstrumentChannels -> Int
chan inst instrumentChans  =
  fromMaybe 0 $ lookup inst instrumentChans


{-
debugMelody :: Melody -> String
debugMelody p = fold $ map debugPhrase p

debugPhrase :: MidiPhrase -> String
debugPhrase p = " phrase -" <> (fold $ map debugNote p)

debugNote :: MidiNote -> String
debugNote n =
  " note: " <> show n.id <> " at offset: " <> show n.timeOffset
-}
