module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Euterpea.Music (PitchClass(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log ( "sample pitch: " <> show C)
