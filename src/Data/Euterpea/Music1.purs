module Data.Euterpea.Music1 where


import Prelude (id, mod, (-), (/))
import Data.Euterpea.Music
import Data.Array ((!!))
import Data.Maybe ( fromMaybe)

data Note1   = Note1 Pitch (Array NoteAttribute)
type Music1  = Music Note1

-- | A new type class to allow for musical polymorphism that ultimately
-- | must be converted to Music1 to be converted to MIDI format through
-- | the MEvent framework.

data PV = PV
  { pitch  :: Pitch
  , volume :: Volume
  }

class ToMusic1 a where
  toMusic1 :: Music a -> Music1

instance p2m1 :: ToMusic1 Pitch where
  toMusic1 =
    mMap (\p -> Note1 p [])

instance pv2m1 :: ToMusic1 PV where
  toMusic1 =
    mMap (\(PV pv) -> Note1 pv.pitch [Volume pv.volume] )

instance m12m1 :: ToMusic1 (Note1) where
  toMusic1 = id

-- Int is AbsPitch but type classes for synonyms are disallowed
instance absp2m1 :: ToMusic1 (Int) where
  toMusic1 =
    mMap (\a -> Note1 (pitch a) [])

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
