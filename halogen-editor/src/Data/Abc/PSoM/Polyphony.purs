module Data.Abc.PSoM.Polyphony (generateDSL) where

import Prelude (($), (>), (<>))
import Data.Array (index, length, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldl)
import Data.Abc (AbcTune, TuneBody)
import Data.Abc.Voice (partitionTuneBody)
import Data.Abc.Metadata (getTitle)
import Data.Midi.Instrument (InstrumentName(..))
import Data.Abc.PSoM.DSL (toDSL)
import Data.Abc.PSoM.Translation (initialise, toPSoM)


-- | Discriminate between monophonic and polyphonic ABC tunes and generate
-- | appropriate PSoM DSL.  The latter requires separate strands for each voice
-- | which are joined together at the start by means of a Par construct

-- | generate the PSoM DSL for the ABC tune using the provided instruments
generateDSL :: AbcTune -> Array InstrumentName -> String
generateDSL  abcTune instrumentNames =
  let
    voices = partitionTuneBody abcTune
    tuneName = entitle $ getTitle abcTune
  in
    if (length voices) > 1 then
      -- polyphonic
      let
        voicesArray :: Array String
        voicesArray = generateVoices abcTune voices instrumentNames
      in
        tuneName <> "\r\n" <> "Par\r\n" <> (foldl (<>) "" voicesArray)
    else
      -- monophonic
      tuneName <> "\r\n" <> (generateVoice instrumentNames abcTune 0 abcTune.body)

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

-- | get the quoted title of the ABC tune ('unttitled' if not title present)
entitle :: Maybe String -> String
entitle Nothing = (enquote "untitled")
entitle (Just name) = (enquote name)

enquote :: String -> String
enquote s =
  "\"" <> s <> "\""
