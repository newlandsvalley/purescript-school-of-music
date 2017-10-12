module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)

import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.DSL (dslSuite)
import Test.Performance (performanceSuite)

main :: forall t.
        Eff
          ( console :: CONSOLE
          , testOutput :: TESTOUTPUT
          , avar :: AVAR
          | t
          )
          Unit
main = runTest do
  suite "euterpea" do
    dslSuite
    performanceSuite
