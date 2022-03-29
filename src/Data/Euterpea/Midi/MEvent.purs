module Data.Euterpea.Midi.MEvent where

-- | Intermediate MIDI representation

import Prelude (class Show, class Eq, class Ord, compare, map, negate, (+), (-), (<), (/), (*), ($), (<>), (<<<))
import Data.List (List(..), (:), head, singleton)
import Data.Int (round)
import Data.Euterpea.Music
import Data.Euterpea.Dynamics (Dynamic(..), StdLoudness(..))
import Data.Euterpea.Music1 (class ToMusic1, Music1, Note1(..), toMusic1)
import Data.Euterpea.Notes (qn, absPitch)
import Data.Euterpea.Transform (rMax, scaleDurations, shiftPitches2)
import Data.Midi.Instrument (InstrumentName(..))
import Data.Rational(Rational, fromInt, toNumber)
import Data.Tuple (Tuple(..), fst)
import Data.Maybe (Maybe(Just))
import Data.Foldable (foldr)
import Data.Generic.Rep as G
import Data.Eq.Generic as GEq
import Data.Show.Generic as GShow


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
instance showMEvent :: Show MEvent where
  show x = GShow.genericShow x
instance ordMEvent:: Ord MEvent where
  compare (MEvent r1) (MEvent r2) =
    compare r1.eTime r2.eTime


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

eVol :: MEvent -> Volume
eVol (MEvent me) = me.eVol

eDur :: MEvent -> DurT
eDur (MEvent me) = me.eDur

zero :: Rational
zero = fromInt 0

one :: Rational
one = fromInt 1

two :: Rational
two = fromInt 2



perform :: ∀ a. (ToMusic1 a) => Music a -> Performance
perform = perform1 <<< toMusic1

perform1 :: Music1 -> Performance
perform1 = fst <<< perform1Dur

perform1Dur :: Music1 -> Tuple Performance DurT
perform1Dur =
  (musicToMEvents defCon) <<< applyControls where
    defCon  = MContext {mcTime : zero, mcInst : AcousticGrandPiano, mcDur : metro 120 qn, mcVol:127}
    -- timing musicToMEventss
    metro :: Int -> Dur -> DurT
    -- metro setting dur  = 60 / (fromIntegral setting * dur)
    metro setting dur  = (fromInt 60) / (fromInt setting * dur)

merge :: Performance -> Performance -> Performance
merge Nil es2 =  es2
merge es1 Nil =  es1
merge a@(e1:es1)  b@(e2:es2)  =
  if eTime e1 < eTime e2  then
    e1 : merge es1 b
  else
    e2 : merge a es2

musicToMEvents :: MContext -> Music1 -> Tuple Performance DurT
musicToMEvents c@(MContext {mcTime:_t, mcDur:dt}) (Prim (Note d p)) =
  Tuple (singleton (noteToMEvent c d p)) (d*dt)
musicToMEvents (MContext {mcTime:_t, mcDur:dt}) (Prim (Rest d)) =
  Tuple Nil (d*dt)
musicToMEvents (MContext c) (m1 :+: m2) =
    let t = c.mcTime
        Tuple evs1 d1 = musicToMEvents (MContext c) m1
        Tuple evs2 d2 = musicToMEvents (MContext (c {mcTime = t+d1 })) m2
    in
      Tuple (evs1 <> evs2) (d1+d2)
musicToMEvents c (m1 :=: m2) =
    let Tuple evs1 d1 = musicToMEvents c m1
        Tuple evs2 d2 = musicToMEvents c m2
    in
      Tuple (merge evs1 evs2) (rMax d1 d2)
musicToMEvents (MContext c) (Modify (Instrument i) m) =
   musicToMEvents (MContext (c { mcInst=i })) m
musicToMEvents c (Modify (Phrase pas) m) =
   phraseToMEvents c pas m
musicToMEvents c (Modify (KeySig _ _) m) =
  musicToMEvents c m            -- KeySig causes no change
musicToMEvents c (Modify (Custom _) m) =
  musicToMEvents c m              -- Custom causes no change
musicToMEvents c m@(Modify _ _) =
  musicToMEvents c $ applyControls m    -- Transpose and Tempo addressed by applyControls

-- | I don't yet understand the original HSoM noteToMEvent shown here
-- | this function sets MEvent charactersitics from the note pitch
-- | together with the current context.  (The context has been set, for example,
-- | by applying the phrase attributes).  However, when it comes to volume
-- | it just ignores the context volume and replaces it with the context from
-- | the note itself
-- | perhaps it would be better if modifying the volume Phrase Attribute provided
-- | a multiplier for the note volume and not simply a replacement
-- | I need to review the original Euterpea documentation


{-  Original HS0M implementation
noteToMEvent :: MContext -> Dur ->  Note1 -> MEvent
noteToMEvent (MContext { mcTime:ct, mcInst:ci, mcDur:cdur, mcVol:cvol }) d (Note1 p nas) =
  let
    e0 = MEvent {eTime:ct, ePitch:absPitch p, eInst:ci, eDur:d*cdur, eVol:cvol, eParams:Nil }
  in
    foldr nasFun e0 nas where
      nasFun :: NoteAttribute -> MEvent -> MEvent
      nasFun (Volume v) (MEvent ev) =
        MEvent (ev {eVol = v})
      nasFun (Params pms) (MEvent ev) =
        MEvent (ev {eParams = pms})
      nasFun _ ev = ev
-}

noteToMEvent :: MContext -> Dur ->  Note1 -> MEvent
noteToMEvent (MContext { mcTime:ct, mcInst:ci, mcDur:cdur, mcVol:cvol }) d (Note1 p _) =
  MEvent {eTime:ct, ePitch:absPitch p, eInst:ci, eDur:d*cdur, eVol:cvol, eParams:Nil }

applyControls :: Music1 -> Music1
applyControls (Modify (Tempo r) m) = scaleDurations r (applyControls m)
applyControls (Modify (Transpose k) m) = shiftPitches2 k (applyControls m)
applyControls (Modify x m) = Modify x $ applyControls m
applyControls (m1 :+: m2) = applyControls m1 :+: applyControls m2
applyControls (m1 :=: m2) = applyControls m1 :=: applyControls m2
applyControls x = x



-- | This is an horrendous function!
phraseToMEvents :: MContext -> List PhraseAttribute -> Music1 -> Tuple Performance DurT
phraseToMEvents c Nil m = musicToMEvents c m
phraseToMEvents c@(MContext mctx) (pa:pas) m =
  let
      {-
      t = mctx.mcTime
      i = mctx.mcInst      
      -- dt = mctx.mcDur ??? not used???
      -}
      pfd           =  phraseToMEvents c pas m
      Tuple pf dur  =  pfd
      loud x        =  phraseToMEvents c (Dyn (Loudness x) : pas) m
      stretch x     =  let
                         t0 = case (head pf) of
                           Just heade -> eTime heade
                           _ -> fromInt 0
                         r  = x/dur
                         upd :: MEvent -> MEvent
                         upd (MEvent e) =
                              let
                                   d = e.eDur
                                   dt = e.eTime - t0
                                   t'  = (one+dt*r)*dt + t0
                                   d'  = (one+(two*dt+d)*r)*d
                              in
                                MEvent (e {eTime = t', eDur = d'})
                       in
                         Tuple (map upd pf) ((one+x)*dur)
      inflate x     =  let
                         t0 = case (head pf) of
                           Just heade -> eTime heade
                           _ -> fromInt 0
                         r   = x/dur
                         -- upd (e@MEvent {eTime : t, eVol : v}) =
                         upd :: MEvent -> MEvent
                         upd (MEvent e) =
                              let
                                   v = e.eVol
                                   v' = round $ toNumber $ (one+(e.eTime-t0)*r) * (fromInt v)
                              in
                                MEvent (e {eVol = v'})
                       in
                         Tuple (map upd pf) dur
  in case pa of
    Dyn (Accent x) ->
      let
        upd :: MEvent -> MEvent
        upd (MEvent e) =
          let
            v = e.eVol
            v' = round $ toNumber (x * (fromInt v))
          in
            MEvent (e {eVol = v'})
      in
        Tuple (map upd pf) dur
    Dyn (StdLoudness l) ->
       case l of
          PPP  -> loud (fromInt 40)
          PP   -> loud (fromInt 50)
          P    -> loud (fromInt 60)
          MP   -> loud (fromInt 70)
          SF   -> loud (fromInt 80)
          MF   -> loud (fromInt 90)
          NF   -> loud (fromInt 100)
          FF   -> loud (fromInt 110)
          FFF  -> loud (fromInt 120)
    Dyn (Loudness x)    ->
      -- phraseToMEvents c{mcVol = round x} pas m
      let
        newCtx = MContext (mctx { mcVol = round $ toNumber x })
      in
        phraseToMEvents newCtx pas m
    Dyn (Crescendo x)   ->  inflate x
    Dyn (Diminuendo x)  ->  inflate (-x)
    Tmp (Ritardando x)  ->  stretch x
    Tmp (Accelerando x) ->  stretch (-x)
    Art (Staccato x)    ->
      let
        upd :: MEvent -> MEvent
        upd (MEvent e) =
          let
            d' = e.eDur * x
          in
            MEvent (e {eDur = d'})
      in
        Tuple (map upd pf) dur
    Art (Legato x)      ->
      let
        upd :: MEvent -> MEvent
        upd (MEvent e) =
          let
            d' = e.eDur * x
          in
            MEvent (e {eDur = d'})
      in
        Tuple (map upd pf) dur
    Art (Slurred x)     ->
       let
         lastStartTime  = foldr (\e tm -> rMax (eTime e) tm) (fromInt 0) pf
         setDur (MEvent e) =
           if (e.eTime) < lastStartTime then
             MEvent (e {eDur = x * e.eDur })
           else
             MEvent e
       in Tuple (map setDur pf) dur
    Art _                -> pfd -- not supported
    Orn _                -> pfd -- not supported
