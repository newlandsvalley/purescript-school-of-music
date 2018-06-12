module Test.Main where

import Prelude
import Effect (Effect)

import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.DSL (dslSuite)
import Test.Performance (performanceSuite)

main :: Effect Unit
main = runTest do
  suite "euterpea" do
    dslSuite
    performanceSuite
