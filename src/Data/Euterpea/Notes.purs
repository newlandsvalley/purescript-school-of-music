module Data.Euterpea.Notes where

import Prelude (negate, (*), (+))
import Data.Euterpea.Music
import Data.Euterpea.Music1 (pitch)
import Data.Midi.Instrument (InstrumentName)
import Data.Rational (fromInt, (%))
import Data.List (List)

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

phrase :: forall a. (List PhraseAttribute) -> Music a -> Music a
phrase pa m = Modify (Phrase pa) m

keysig :: forall a. PitchClass -> Mode -> Music a -> Music a
keysig pc mo m = Modify (KeySig pc mo) m

trans :: Int -> Pitch -> Pitch
trans i p  = pitch (absPitch p + i)

-- | note constructors
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

-- | duration constructors

-- | brevis
bn :: Dur
bn = fromInt 2

bnr :: Music Pitch
bnr = rest bn

-- | whole note
wn :: Dur
wn = fromInt 1

wnr :: Music Pitch
wnr = rest wn

-- | half note
hn :: Dur
hn = 1 % 2

hnr :: Music Pitch
hnr = rest hn

-- | quarter note
qn :: Dur
qn = 1 % 4

qnr :: Music Pitch
qnr = rest qn

-- | eighth note
en :: Dur
en = 1 % 8

enr :: Music Pitch
enr = rest en

-- | sixteenth note
sn :: Dur
sn = 1 % 16

snr :: Music Pitch
snr = rest sn

-- | thirtysecond note
tn :: Dur
tn = 1 % 32

tnr :: Music Pitch
tnr = rest tn

-- | sixtyfourth note
sfn :: Dur
sfn = 1 % 32

sfnr :: Music Pitch
sfnr = rest sfn

-- | dotted whole note
dwn :: Dur
dwn = 3 % 2

dwnr :: Music Pitch
dwnr = rest dwn

-- | dotted half note
dhn :: Dur
dhn = 3 % 4

dhnr :: Music Pitch
dhnr = rest dhn

-- | dotted quarter note
dqn :: Dur
dqn = 3 % 8

dqnr :: Music Pitch
dqnr = rest dqn

-- | dotted eighth note
den :: Dur
den = 3 % 16

denr :: Music Pitch
denr = rest den

-- | dotted sixteenth note
dsn :: Dur
dsn = 3 % 32

dsnr :: Music Pitch
dsnr = rest dsn

-- | dotted thirtysecond note
dtn :: Dur
dtn = 3 % 64

dtnr :: Music Pitch
dtnr = rest dtn

-- | double-dotted half note
ddhn :: Dur
ddhn = 7 % 8

ddhnr :: Music Pitch
ddhnr = rest ddhn

-- | double-dotted quarter note
ddqn :: Dur
ddqn = 7 % 16

ddqnr :: Music Pitch
ddqnr = rest ddqn

-- | double-dotted eigthth note
dden :: Dur
dden = 7 % 32

ddenr :: Music Pitch
ddenr = rest dden


pcToInt :: PitchClass -> Int
pcToInt pc  = case pc of
  Cff  -> -2
  Cf  -> -1
  C  -> 0
  Cs  -> 1
  Css  -> 2
  Dff  -> 0
  Df  -> 1
  D  -> 2
  Ds  -> 3
  Dss  -> 4
  Eff  -> 2
  Ef  -> 3
  E  -> 4
  Es  -> 5
  Ess  -> 6
  Fff  -> 3
  Ff  -> 4
  F  -> 5
  Fs  -> 6
  Fss  -> 7
  Gff  -> 5
  Gf  -> 6
  G  -> 7
  Gs  -> 8
  Gss  -> 9
  Aff  -> 7
  Af  -> 8
  A  -> 9
  As  -> 10
  Ass  -> 11
  Bff  -> 9
  Bf  -> 10
  B  -> 11
  Bs  -> 12
  Bss  -> 13

-- Note:  HSoM uses middle C = MIDI 60 = (C, 4)

absPitch :: Pitch -> AbsPitch
absPitch (Pitch pc oct)  = 12*(oct+1) + pcToInt pc
