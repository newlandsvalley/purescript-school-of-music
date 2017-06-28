module Data.Euterpea.Notes where

import Data.Euterpea.Music

note :: forall a. Dur -> a -> Music a
note du p = Prim (Note du p)

rest :: forall a. Dur -> Music a
rest du = Prim (Rest du)

tempo :: forall a. Dur -> Music a -> Music a
tempo r m = Modify (Tempo r) m

transpose :: forall a. AbsPitch -> Music a -> Music a
transpose i m = Modify (Transpose i) m

instrument :: forall a. InstrumentName -> Music a -> Music a
instrument i m = Modify (Instrument i) m

phrase :: forall a. (Array PhraseAttribute) -> Music a -> Music a
phrase pa m = Modify (Phrase pa) m

keysig :: forall a. PitchClass -> Mode -> Music a -> Music a
keysig pc mo m = Modify (KeySig pc mo) m

cff :: Octave -> Dur -> Music Pitch
cff o du = note du (Pitch Cff o)

cf :: Octave -> Dur -> Music Pitch
cf o du = note du (Pitch Cf o)

c :: Octave -> Dur -> Music Pitch
c o du = note du (Pitch C o)

cs :: Octave -> Dur -> Music Pitch
cs o du = note du (Pitch Cs o)

css :: Octave -> Dur -> Music Pitch
css o du = note du (Pitch Css o)

dff :: Octave -> Dur -> Music Pitch
dff o du = note du (Pitch Dff o)

df :: Octave -> Dur -> Music Pitch
df o du = note du (Pitch Df o)

d :: Octave -> Dur -> Music Pitch
d o du = note du (Pitch D o)

ds :: Octave -> Dur -> Music Pitch
ds o du = note du (Pitch Ds o)

dss :: Octave -> Dur -> Music Pitch
dss o du = note du (Pitch Dss o)

eff :: Octave -> Dur -> Music Pitch
eff o du = note du (Pitch Eff o)

ef :: Octave -> Dur -> Music Pitch
ef o du = note du (Pitch Ef o)

e :: Octave -> Dur -> Music Pitch
e o du = note du (Pitch E o)

es :: Octave -> Dur -> Music Pitch
es o du = note du (Pitch Es o)

ess :: Octave -> Dur -> Music Pitch
ess o du = note du (Pitch Ess o)

fff :: Octave -> Dur -> Music Pitch
fff o du = note du (Pitch Fff o)

ff :: Octave -> Dur -> Music Pitch
ff o du = note du (Pitch Ff o)

f :: Octave -> Dur -> Music Pitch
f o du = note du (Pitch F o)

fs :: Octave -> Dur -> Music Pitch
fs o du = note du (Pitch Fs o)

fss :: Octave -> Dur -> Music Pitch
fss o du = note du (Pitch Fss o)

gff :: Octave -> Dur -> Music Pitch
gff o du = note du (Pitch Gff o)

gf :: Octave -> Dur -> Music Pitch
gf o du = note du (Pitch Gf o)

g :: Octave -> Dur -> Music Pitch
g o du = note du (Pitch G o)

gs :: Octave -> Dur -> Music Pitch
gs o du = note du (Pitch Gs o)

gss :: Octave -> Dur -> Music Pitch
gss o du = note du (Pitch Gss o)

aff :: Octave -> Dur -> Music Pitch
aff o du = note du (Pitch Aff o)

af :: Octave -> Dur -> Music Pitch
af o du = note du (Pitch Af o)

a :: Octave -> Dur -> Music Pitch
a o du = note du (Pitch A o)

as :: Octave -> Dur -> Music Pitch
as o du = note du (Pitch As o)

ass :: Octave -> Dur -> Music Pitch
ass o du = note du (Pitch Ass o)

bff :: Octave -> Dur -> Music Pitch
bff o du = note du (Pitch Bff o)

bf :: Octave -> Dur -> Music Pitch
bf o du = note du (Pitch Bf o)

b :: Octave -> Dur -> Music Pitch
b o du = note du (Pitch B o)

bs :: Octave -> Dur -> Music Pitch
bs o du = note du (Pitch Bs o)

bss :: Octave -> Dur -> Music Pitch
bss o du = note du (Pitch Bss o)
