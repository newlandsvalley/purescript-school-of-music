module Data.Euterpea.Transform where

import Prelude (const, flip, map, (+), (-), (*), (/), (>), (<), (<>), (<=), ($))
import Data.Euterpea.Music
import Data.Euterpea.Music1 (Note1(..), mMap, pitch)
import Data.Euterpea.Notes (absPitch, note, rest, tempo, trans)
import Data.Midi.Instrument (InstrumentName)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)
import Data.Rational (Rational, fromInt, toNumber)
import Data.Tuple (Tuple(..))
import Data.List (List(..), head, last, singleton, (:))
import Data.List.NonEmpty as Nel

-- | utility functions that should probably belong in various Purescript libraries
foldr1 :: ∀ a . (a -> a -> a) -> Nel.NonEmptyList a -> a
foldr1 f as =
  let
    { init, last } = Nel.unsnoc as
  in
    foldr f last init

rMax :: Rational -> Rational -> Rational
rMax r1 r2 =
  if
    toNumber r1 > toNumber r2
  then
    r1
  else
    r2

rMin :: Rational -> Rational -> Rational
rMin r1 r2 =
  if
    toNumber r1 < toNumber r2
  then
    r1
  else
    r2


-- | MoreMusic proper

-- not sure what the fixity should be here
infixl 5 cutm as /=:

line :: ∀ a. List (Music a) -> Music a
line = foldr (:+:) (rest (fromInt 0))

chord :: ∀ a. List (Music a) -> Music a
chord = foldr (:=:) (rest (fromInt 0))

line1 :: ∀ a. Nel.NonEmptyList (Music a) -> Music a
line1 = foldr1 (:+:)

chord1 :: ∀ a. Nel.NonEmptyList (Music a) -> Music a
chord1 = foldr1 (:=:)

offset :: ∀ a. Dur -> Music a -> Music a
offset du m  = rest du :+: m

times :: ∀ a. Int -> Music a -> Music a
times 0 m  = rest (fromInt 0)
times n m  = m :+: times (n-1) m


{- can we do this with Lazy?
forever :: ∀ a . Music a -> Music a
forever m  = m :+: forever m
-}

-- | how do I pattern match on Rationals?
-- | how do I return an error?
lineToList :: ∀ a. Music a -> List (Music a)
lineToList (Prim (Rest _)) = Nil
lineToList (n :+: ns) = n : lineToList ns
lineToList _ =
     -- error "lineToList: argument not created by function line"
     Nil

invertAt :: Pitch -> Music Pitch -> Music Pitch
invertAt pRef = mMap (\p -> pitch (2 * absPitch pRef - absPitch p))

invertAt1 :: ∀ a. Pitch -> Music (Tuple Pitch a) -> Music (Tuple Pitch a)
invertAt1 pRef = mMap (\(Tuple p x) -> Tuple (pitch (2 * absPitch pRef - absPitch p)) x)

invert :: Music Pitch -> Music Pitch
invert m =
  let
    pRef = mFold pFun (<>) (<>) (flip const) m
  in
    case (head pRef) of
      Just pr ->
        invertAt pr m
      _ ->
        m
  where
    pFun (Note _ p) = singleton p
    pFun _ = Nil

invert1 :: ∀ a. Music (Tuple Pitch a) -> Music (Tuple Pitch a)
invert1 m =
  let
    pRef = mFold pFun (<>) (<>) (flip const) m
  in
    case (head pRef) of
      Just pr ->
        invertAt1 pr m
      _ ->
        m
  where
    pFun (Note d (Tuple p _)) = singleton p
    pFun _ = Nil

retro :: ∀ a. Music a -> Music a
retro n@(Prim _)    = n
retro (Modify c m)  = Modify c (retro m)
retro (m1 :+: m2)   = retro m2 :+: retro m1
retro (m1 :=: m2)   =
   let
     d1 = dur m1
     d2 = dur m2
   in
     if d1>d2 then
       retro m1 :=: (rest (d1-d2) :+: retro m2)
     else
       (rest (d2-d1) :+: retro m1) :=: retro m2

dur :: ∀ a. Music a -> Dur
dur (Prim (Note d _))     = d
dur (Prim (Rest d))       = d
dur (m1 :+: m2)           = dur m1   +   dur m2
dur (m1 :=: m2)           = dur m1 `rMax` dur m2
dur (Modify (Tempo r) m)  = dur m / r
dur (Modify _ m)          = dur m

cut :: ∀ a. Dur -> Music a -> Music a
cut d m | d <= (fromInt 0)  = rest (fromInt 0)
cut d (Prim (Note oldD p))  = note (rMin oldD d) p
cut d (Prim (Rest oldD))    = rest (rMin oldD d)
cut d (m1 :=: m2)           = cut d m1 :=: cut d m2
cut d (m1 :+: m2) =
  let
    m'1  = cut d m1
    m'2  = cut (d - dur m'1) m2
  in
    m'1 :+: m'2
cut d (Modify (Tempo r) m)  = tempo r (cut (d*r) m)
cut d (Modify c m)          = Modify c (cut d m)

remove :: ∀ a. Dur -> Music a -> Music a
remove d m | d <= (fromInt 0)  = m
remove d (Prim (Note oldD p))  = note (rMax (oldD-d) (fromInt 0)) p
remove d (Prim (Rest oldD))    = rest (rMax (oldD-d) (fromInt 0))
remove d (m1 :=: m2)           = remove d m1 :=: remove d m2
remove d (m1 :+: m2) =
  let
    m'1  = remove d m1
    m'2  = remove (d - dur m1) m2
  in
    m'1 :+: m'2
remove d (Modify (Tempo r) m)  = tempo r (remove (d*r) m)
remove d (Modify c m)          = Modify c (remove d m)

removeZeros :: ∀ a. Music a -> Music a
removeZeros (Prim p)      = Prim p
removeZeros (ml :+: mr)   =
  let
    m'1  = removeZeros ml
    m'2  = removeZeros mr
  in
    case (Tuple m'1 m'2) of
       Tuple (Prim (Note d p)) m | d <= (fromInt 0)  -> m
       Tuple (Prim (Rest d)) m | d <= (fromInt 0)    -> m
       Tuple m (Prim (Note d p)) | d <= (fromInt 0)  -> m
       Tuple m (Prim (Rest d)) |  d <= (fromInt 0)   -> m
       Tuple m1 m2                          -> m1 :+: m2
removeZeros (ml :=: mr)   =
  let
    m'1  = removeZeros ml
    m'2  = removeZeros mr
  in
    case (Tuple m'1 m'2) of
       Tuple (Prim (Note d p)) m | d <= (fromInt 0) -> m
       Tuple (Prim (Rest d)) m   | d <= (fromInt 0) -> m
       Tuple m (Prim (Note d p)) | d <= (fromInt 0) -> m
       Tuple m (Prim (Rest d))   | d <= (fromInt 0) -> m
       Tuple m1 m2                -> m1 :=: m2
removeZeros (Modify c m)  = Modify c (removeZeros m)

type LazyDur = List Dur
durL :: ∀ a. Music a -> LazyDur
durL m@(Prim _)  =  singleton (dur m)
durL (m1 :+: m2) =
  let
    d1 = durL m1
    f = case (last d1) of
      Just lastd -> (+) lastd
      _ -> (+) (fromInt 0)
  in
    d1 <> map f (durL m2)
durL (m1 :=: m2) =
  mergeLD (durL m1) (durL m2)
durL (Modify (Tempo r) m)  =
  let
    f = (/) r
  in
    map f (durL m)
durL (Modify _ m) =  durL m

mergeLD :: LazyDur -> LazyDur -> LazyDur
mergeLD Nil ld = ld
mergeLD ld Nil = ld
mergeLD ld1@(d1:ds1) ld2@(d2:ds2) =
  if d1<d2 then
    d1 : mergeLD ds1 ld2
  else
    d2 : mergeLD ld1 ds2

minL :: LazyDur -> Dur -> Dur
minL Nil d' = d'
minL (d:Nil) d' = rMin d d'
minL (d:ds) d' =
  if d < d' then
    minL ds d'
  else
    d'

cutL :: ∀ a. LazyDur -> Music a -> Music a
cutL Nil _                       = rest (fromInt 0)
cutL (d:ds) m | d <= (fromInt 0) = cutL ds m
cutL ld (Prim (Note oldD p))     = note (minL ld oldD) p
cutL ld (Prim (Rest oldD))       = rest (minL ld oldD)
cutL ld (m1 :=: m2)              = cutL ld m1 :=: cutL ld m2
cutL ld (m1 :+: m2) =
   let
     m'1 = cutL ld m1
     m'2 = cutL (map (\d -> d - dur m'1) ld) m2
   in
     m'1 :+: m'2
cutL ld (Modify (Tempo r) m)  =
  let
    f = (*) r
  in
    tempo r (cutL (map f ld) m)
cutL ld (Modify c m)              = Modify c (cutL ld m)

cutm :: ∀ a. Music a -> Music a -> Music a
cutm m1 m2 = cutL (durL m2) m1 :=: cutL (durL m1) m2



mFold :: ∀ a b. (Primitive a -> b) -> (b->b->b) -> (b->b->b) ->
           (Control -> b -> b) -> Music a -> b
mFold f seqf parf g m =
  let
    rec = mFold f seqf parf g
  in
    case m of
      Prim p      -> f p
      m1 :+: m2   -> seqf (rec m1)  (rec m2)
      m1 :=: m2   -> parf (rec m1)  (rec m2)
      Modify c mus  -> g c (rec mus)

-- | Sometimes we may wish to alter the internal structure of a Music value
-- | rather than wrapping it with Modify. The following functions allow this.

shiftPitches :: AbsPitch -> Music Pitch -> Music Pitch
shiftPitches k = mMap (trans k)

shiftPitches2 :: AbsPitch -> Music Note1 -> Music Note1
shiftPitches2 k = mMap (\(Note1 p xs) -> Note1 (trans k p) xs)

-- | shiftPitches2 is useless unless we model Note1 as a Tuple
shiftPitches1 :: ∀ a. AbsPitch -> Music (Tuple Pitch a) -> Music (Tuple Pitch a)
shiftPitches1 k = mMap (\(Tuple p xs) -> Tuple (trans k p) xs)

scaleDurations :: ∀ a. Rational -> Music a -> Music a
scaleDurations r (Prim (Note d p)) = note (d/r) p
scaleDurations r (Prim (Rest d)) = rest (d/r)
scaleDurations r (m1 :+: m2) = scaleDurations r m1 :+: scaleDurations r m2
scaleDurations r (m1 :=: m2) = scaleDurations r m1 :=: scaleDurations r m2
scaleDurations r (Modify c m) = Modify c (scaleDurations r m)

changeInstrument :: ∀ a. InstrumentName -> Music a -> Music a
changeInstrument i m = Modify (Instrument i) $ removeInstruments m

removeInstruments :: ∀ a. Music a -> Music a
removeInstruments (Modify (Instrument _) m) = removeInstruments m
removeInstruments (Modify c m) = Modify c $ removeInstruments m
removeInstruments (m1 :+: m2) = removeInstruments m1 :+: removeInstruments m2
removeInstruments (m1 :=: m2) = removeInstruments m1 :=: removeInstruments m2
removeInstruments m = m
