module Data.Euterpea.Transform where

import Prelude (const, flip, (+), (-), (*), (/), (>), (<>))
import Data.Euterpea.Music
import Data.Euterpea.Music1 (mMap, pitch)
import Data.Euterpea.Notes (absPitch, rest)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)
import Data.Rational (Rational, fromInt, toNumber)
import Data.Tuple (Tuple(..))
import Data.List (List(..), head, singleton, (:))
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

-- | MoreMusic proper

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
lineToList (Prim (Rest du)) = Nil
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
    pFun (Note d p) = singleton p
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
    pFun (Note d (Tuple p x)) = singleton p
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
