module Data.Euterpea.Music1 where


import Prelude (class Show, identity, mod, (-), (/))
import Data.Euterpea.Music
import Data.List (List(..), (:))
import Data.Array ((!!))
import Data.Maybe ( fromMaybe)
import Data.Generic.Rep as G
import Data.Show.Generic as GShow


-- | We have difficulty with polymorphism in Music here
-- | Euterpea uses a Tuple as the means of expressing
-- | polymorphic values with the second element indicating the polymorphic value
-- | However Purescript disallows type classes for Type Synonyms such as Tuples
-- | Instead we use a data definition for Note1 which is not extensible polymorphically
data Note1   = Note1 Pitch (List NoteAttribute)
type Music1  = Music Note1

-- | A new type class to allow for musical polymorphism that ultimately
-- | must be converted to Music1 to be converted to MIDI format through
-- | the MEvent framework.

derive instance genericMode :: G.Generic Note1  _
{-}
instance eqNote1  :: Eq Note1 where
  eq x y = GEq.genericEq x y
instance ordNote1  :: Ord Note1  where
  compare x y = GOrd.genericCompare x y
-}
instance showNote1  :: Show Note1  where
  show x = GShow.genericShow x

data PV = PV Pitch Volume

class ToMusic1 a where
  toMusic1 :: Music a -> Music1

instance p2m1 :: ToMusic1 Pitch where
  toMusic1 =
    mMap (\p -> Note1 p Nil)

instance pv2m1 :: ToMusic1 PV where
  toMusic1 =
    mMap (\(PV p v) -> Note1 p ((Volume v) : Nil) )

instance m12m1 :: ToMusic1 (Note1) where
  toMusic1 = identity

-- Int is AbsPitch but type classes for synonyms are disallowed
instance absp2m1 :: ToMusic1 (Int) where
  toMusic1 =
    mMap (\a -> Note1 (pitch a) Nil)

pMap :: forall a b. (a -> b) -> Primitive a -> Primitive b
pMap f (Note d x)  = Note d (f x)
pMap f (Rest d)    = Rest d

mMap :: forall a b. (a -> b) -> Music a -> Music b
mMap f (Prim p)      = Prim (pMap f p)
mMap f (m1 :+: m2)   = mMap f m1 :+: mMap f m2
mMap f (m1 :=: m2)   = mMap f m1 :=: mMap f m2
mMap f (Modify c m)  = Modify c (mMap f m)

pitch :: AbsPitch -> Pitch
pitch ap  =
  let
    oct = ap / 12
    n = ap `mod` 12
    pc = fromMaybe C ([C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] !! n)
  in
    Pitch pc (oct-1)
