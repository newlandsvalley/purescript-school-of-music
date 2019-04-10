module Data.Abc.PSoM.Polyphony (generateDSL) where

import Prelude (($), (>), (<>))
import Data.Array (index, length, mapWithIndex)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl)
import Data.Abc (AbcTune, TuneBody)
import Data.Abc.Voice (partitionTuneBody)
import Data.Midi.Instrument (InstrumentName(..))
import Data.Abc.PSoM.DSL (toDSL)
import Data.Abc.PSoM.Translation (initialise, toPSoM)


-- | Discriminate between monophonic and polyphonic ABC tunes and generate
-- | appropriate PSoM DSL.  The latter requires separate strands for each voice
-- | which are joined together at the start by means of a Par construct

generateDSL :: AbcTune -> Array InstrumentName -> String
generateDSL  abcTune instrumentNames =
  let
    voices = partitionTuneBody abcTune.body
  in
    if (length voices) > 1 then
      -- polyphonic
      let
        voicesArray :: Array String
        voicesArray = generateVoices abcTune voices instrumentNames
      in
        "Par\r\n" <> (foldl (<>) "" voicesArray)
    else
      -- monophonic
      generateVoice instrumentNames abcTune 0 abcTune.body

-- | generate the DSL for all the polyphonic voices
generateVoices :: AbcTune -> Array TuneBody -> Array InstrumentName -> Array String
generateVoices abcTune tuneBodies instrumentNames =
  mapWithIndex (generateVoice instrumentNames abcTune) tuneBodies

-- | generate PSoM DSL for a single voice
-- | note that for monophonic tunes, TuneBody is identical to AbcTune.body
-- | but for polyohonic tunes it is not
-- | (Slightly awkward because initialise only uses the headers but the API
-- | from Abc is expressed in terms of the overall tune.)
generateVoice :: Array InstrumentName -> AbcTune -> Int -> TuneBody -> String
generateVoice instrumentNames abcTune ix  tuneBody =
  let
    transformationState = initialise abcTune
    instrumentName = fromMaybe AcousticGrandPiano $
      index instrumentNames ix
  in
    toDSL (toPSoM tuneBody transformationState) instrumentName
