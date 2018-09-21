module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Browser.Cookies.DataSpec as DataSpec

main :: Effect Unit
main = runTest do
  suite "Cookies" do
    DataSpec.encodingTests
    
