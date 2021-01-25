-- | Handle building PSoM for repeat sections of ABC tunes - 
-- | either simple repeats or voltas
module Data.Abc.PSoM.RepeatBuilder (buildRepeatedMelody) where

import Prelude (($), (<>), (&&), (||), (>), (>=), (<), (<=), (+))
import Data.Array (concat, mapWithIndex, toUnfoldable) as Array
import Data.Foldable (foldl)
import Data.Unfoldable (replicate)
import Data.List (List, (:), null, concatMap, filter, reverse, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Rational (fromInt)
import Data.Abc.Repeats.Types (Section(..), Sections)
import Data.Abc.Repeats.Variant (activeVariants, variantEndingOf, variantCount, variantIndexMax)
import Data.Abc.PSoM
import Data.Abc.PSoM.Types (PSoMBar)

-- | build any repeated section into an extended melody with all repeats realised 
buildRepeatedMelody :: List PSoMBar -> Sections -> PSoMProgram
buildRepeatedMelody mbs sections =
   -- trace "Sections" \_ ->
   -- traceShow sections \_ ->
   if (null sections) then
      mempty
    else
      foldl (repeatedSection mbs) mempty sections

-- | accumulate the PSoM messages from the List of bars
accumulateMessages :: List PSoMBar -> List PSMusic
accumulateMessages mbs =
  reverse $ concatMap _.psomMessages mbs

-- | select a subset of PSoM bars
barSelector :: Int -> Int -> PSoMBar -> Boolean
barSelector strt fin mb =
  mb.number >= strt && mb.number < fin

simpleSlice :: Int -> Int -> List PSoMBar -> List PSMusic
simpleSlice start finish mbs =
  accumulateMessages $ filter (barSelector start finish) mbs

-- | build a (possibly repeated) simple slice
-- | This generates a single variable for the slice
-- | and the program references it more than once (if repeated)
-- | or just once otherwise
trackSlice :: Int -> Int -> Int -> List PSoMBar -> PSoMProgram
trackSlice start finish replicationCount mbs =
  let
    newVar = simpleSlice start finish mbs
    variables = singleton newVar
    program = 
      if (replicationCount > 1) then
        -- replication
        replicate replicationCount 0
      else 
        -- no replication
        Array.toUnfoldable [ 0 ]
    tempo = fromInt 1
  in
    if (null newVar)
      then
        mempty
      else
        PSoMProgram
          { variables, program, tempo }

-- | generate all the slices for variant endings (voltas) in the section 
-- | these are ususally double - e.g.
-- |
-- | ..|1...:|2....||
-- |
-- | but can also be multiple - e.g. 
-- |
-- | ..|1,3....:|2,4....|
-- | 
-- | we thus need a preface slice (from the start to |1)
-- | and a slice for each repeat end
variantSlices :: List PSoMBar -> Section -> PSoMProgram
variantSlices mbs section =
  case section of 
    Section { start: Just start, end: Just end } ->    
      let 
        prefSlice = prefaceSlice mbs start end section 
        endingSlices = accumulateEndingSlices mbs start end section
      in 
        variantProgram prefSlice endingSlices
    _ -> 
      mempty

-- | glue the variant slices together
variantProgram ::  List PSMusic -> Array (List PSMusic) -> PSoMProgram
variantProgram preface endings =
  let 
    endingsList :: List (List PSMusic)
    endingsList = Array.toUnfoldable endings
    variables = preface : endingsList
    programArray :: Array Int
    programArray = Array.concat $ Array.mapWithIndex (\i n -> [0, (i+1)]) endings 
    program :: List Int
    program = Array.toUnfoldable programArray
    tempo = fromInt 1
  in 
    PSoMProgram 
      {variables, program, tempo}

-- | build the preface slice
prefaceSlice :: List PSoMBar -> Int -> Int -> Section -> List PSMusic
prefaceSlice mbs start end section = 
  let
    sectionBars :: List PSoMBar
    sectionBars = filter (barSelector start end) mbs
    firstEnding :: Int
    firstEnding = fromMaybe start $ variantEndingOf 0 section
  in
    simpleSlice start firstEnding sectionBars 

-- | accumulate all the slices for the variant endings
accumulateEndingSlices :: List PSoMBar -> Int -> Int -> Section -> Array (List PSMusic)
accumulateEndingSlices mbs start end section  = 
  let 
    sectionBars :: List PSoMBar
    sectionBars = filter (barSelector start end) mbs
  in 
    Array.mapWithIndex
              (variantEndingSlice start end section sectionBars)
              (activeVariants section)   

-- | build an ending slice for a particular variant ending at index 'index'
variantEndingSlice :: Int -> Int -> Section -> List PSoMBar -> Int -> Int -> List PSMusic 
variantEndingSlice start end section sectionBars index pos = 
  let
    -- the first ending is the main tune section which is always from the 
    -- start to the first volta 
    firstEnding :: Int
    firstEnding = fromMaybe start $ variantEndingOf 0 section
    -- this is the current volta we're looking at
    thisEnding = pos
    -- this next bit is tricky
    --
    -- In the case of 
    --
    --     ..|1 ..:|2 ..:|3 ..:|4 ....
    --
    -- then each variant takes as its ending the start of the next variant
    -- except for the final one which must take the end of the enire section.
    --
    -- In the case of 
    -- 
    --     ..|1,3 ..:|2,4 ....
    --
    -- then this is true, except that also variant 2 must take its ending 
    -- as the end of the entire section.
    -- 
    -- We thus find a candidate ending for the volta (which may not exist).
    -- We'll use it for any variant other than the last, buut reject it in
    -- favour of end if the resulting bar position falls before the start
    -- position of the variant.
    candidateNextEnding = 
      fromMaybe start $ variantEndingOf (index + 1) section
    nextEnding :: Int
    nextEnding =     
      if (index >= variantIndexMax section || candidateNextEnding <= pos)
        then 
          end
        else 
          candidateNextEnding
  in
    simpleSlice thisEnding nextEnding sectionBars      

-- | build a repeat section
-- | this function is intended for use within foldl
repeatedSection ::  List PSoMBar -> PSoMProgram -> Section -> PSoMProgram
repeatedSection mbs acc section = 
  if (variantCount section > 1) then 
    (variantSlices mbs section) <> acc
  else 
    simpleRepeatedSection mbs acc section    

-- | Build simple repeated sections with no variants
-- | but possibly with repeats - e.g.
-- |
-- | |:...|....:|
simpleRepeatedSection :: List PSoMBar -> PSoMProgram -> Section -> PSoMProgram
-- no repeats
simpleRepeatedSection mbs acc (Section { start: Just a, end: Just d, repeatCount : 0 }) =
  (trackSlice a d 0 mbs ) <> acc
-- a repeated section
simpleRepeatedSection mbs acc (Section { start: Just a,  end: Just d, repeatCount : n }) =
  let 
    slices = trackSlice a d (n+1) mbs 
  in 
    slices <> acc
-- something else (unexpected)
simpleRepeatedSection _ acc _ =
  acc


