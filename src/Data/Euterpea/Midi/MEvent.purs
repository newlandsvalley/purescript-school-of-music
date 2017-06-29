module Data.Euterpea.Midi.MEvent where

-- | Intermediate MIDI representation

import Prelude (class Show, class Eq, class Ord, (<), (/), (*))
import Data.List (List(..), (:), singleton)
import Data.Euterpea.Music (AbsPitch, Dur, InstrumentName(..), Music, Volume)
import Data.Euterpea.Music1 (class ToMusic1, Music1)
import Data.Euterpea.Notes (qn)
import Data.Rational(Rational(..), fromInt)
import Data.Tuple (Tuple(..), fst)
import Data.Generic.Rep as G
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow

type PTime = Rational
type DurT = Rational


data MEvent = MEvent {
   eTime    :: PTime,           -- onset time
   eInst    :: InstrumentName,  -- instrument
   ePitch   :: AbsPitch,        -- pitch number
   eDur     :: DurT,            -- note duration
   eVol     :: Volume,          -- volume
   eParams  :: List Number      -- optional other parameters
   }

derive instance genericMEvent :: G.Generic MEvent _
instance eqMEvent :: Eq MEvent where
  eq x y = GEq.genericEq x y
instance ordMEvent :: Ord MEvent where
  compare x y = GOrd.genericCompare x y
instance showMEvent :: Show MEvent where
  show x = GShow.genericShow x


type Performance = List MEvent

data MContext = MContext {
   mcTime    :: PTime,
   mcInst    :: InstrumentName,
   mcDur     :: DurT,
   mcVol     :: Volume
   }

derive instance genericMContext :: G.Generic MContext _
instance showMContext :: Show MContext where
  show x = GShow.genericShow x

-- | where is this defined in Haskell Euterpea ?
eTime :: MEvent -> PTime
eTime (MEvent me) = me.eTime

merge :: Performance -> Performance -> Performance
merge Nil es2 =  es2
merge es1 Nil =  es1
merge a@(e1:es1)  b@(e2:es2)  =
  if eTime e1 < eTime e2  then
    e1 : merge es1 b
  else
    e2 : merge a es2
    
{- not finished
perform :: ∀ a. (ToMusic1 a) => Music a -> Performance
perform = perform1 . toMusic1

perform1 :: Music1 -> Performance
perform1 = fst . perform1Dur

perform1Dur :: Music1 -> Tuple Performance DurT
perform1Dur = musicToMEvents defCon . applyControls where
    defCon  = MContext {mcTime = 0, mcInst = AcousticGrandPiano, mcDur = metro 120 qn, mcVol=127}
    -- timing musicToMEventss
    metro :: Int -> Dur -> DurT
    -- metro setting dur  = 60 / (fromIntegral setting * dur)
    metro setting dur  = 60 / (fromInt setting * dur)

musicToMEvents :: MContext -> Music1 -> Tuple Performance DurT
musicToMEvents c@(MContext {mcTime:t, mcDur:dt}) (Prim (Note d p)) = Tuple (singleton (noteToMEvent c d p)) (d*dt)
musicToMEvents (c@MContext {mcTime:t, mcDur:dt}) (Prim (Rest d)) = Tuple Nil d*dt
musicToMEvents (c@MContext {mcTime:t, mcDur:dt}) (m1 :+: m2) =
    let Tuple evs1 d1 = musicToMEvents c m1
        Tuole evs2 d2 = musicToMEvents c {mcTime = t+d1} m2
    in  Tuple (evs1 ++ evs2) d1+d2
musicToMEvents c@MContext{mcTime:t, mcDur:dt} (m1 :=: m2) =
    let Tuple evs1 d1 = musicToMEvents c m1
        Tuple evs2 d2 = musicToMEvents c m2
    in  Tuple (merge evs1 evs2) max d1 d2
musicToMEvents c (Modify (Instrument i) m) = musicToMEvents c { mcInst=i } m
musicToMEvents c (Modify (Phrase pas) m) = phraseToMEvents c pas m
musicToMEvents c (Modify (KeySig x y) m) = musicToMEvents c m           -- KeySig causes no change
musicToMEvents c (Modify (Custom x) m) = musicToMEvents c m             -- Custom causes no change
musicToMEvents c m@(Modify x m') = musicToMEvents c $ applyControls m    -- Transpose and Tempo addressed by applyControls

-}
