module Data.Euterpea.Dynamics
  ( Dynamic(..)
  , StdLoudness(..)
  , read
  ) where


import Prelude (class Show, class Eq, class Ord, show)
import Data.Rational (Rational)
import Data.Tuple (Tuple(..))
import Data.Map (Map, fromFoldable, lookup) as Map
import Data.Maybe (Maybe)
import Data.Generic.Rep as G
import Data.Eq.Generic as GEq
import Data.Ord.Generic as GOrd
import Data.Show.Generic as GShow

data Dynamic  =
    Accent Rational
  | Crescendo Rational
  | Diminuendo Rational
  | StdLoudness StdLoudness
  | Loudness Rational

derive instance genericDynamic :: G.Generic Dynamic _
instance eqDynamic :: Eq Dynamic where
  eq x y = GEq.genericEq x y
instance ordDynamic :: Ord Dynamic where
  compare x y = GOrd.genericCompare x y
instance showDynamic :: Show Dynamic where
  show x = GShow.genericShow x

data StdLoudness = PPP | PP | P | MP | SF | MF | NF | FF | FFF

derive instance genericStdLoudness :: G.Generic StdLoudness _
instance eqStdLoudness :: Eq StdLoudness where
  eq x y = GEq.genericEq x y
instance ordStdLoudness :: Ord StdLoudness where
  compare x y = GOrd.genericCompare x y
instance showStdLoudness :: Show StdLoudness where
  show x = GShow.genericShow x

-- | again - there's no generic deriving for Read or Enum
-- | at the moment in purescript.  Replace this if and when it arrives.
names :: Map.Map String StdLoudness
names =
 Map.fromFoldable
   [
     Tuple (show PPP ) PPP
   , Tuple (show PP ) PP
   , Tuple (show P ) P
   , Tuple (show MP ) MP
   , Tuple (show SF ) SF
   , Tuple (show MF ) MF
   , Tuple (show NF ) NF
   , Tuple (show FF ) FF
   , Tuple (show FFF ) FFF
   ]

read :: String -> Maybe StdLoudness
read g =
  Map.lookup g names
