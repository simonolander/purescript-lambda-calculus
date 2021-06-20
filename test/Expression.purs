module Test.Expression where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Unit (suite)
import Test.Unit (test)
import Test.Unit.Assert (assertFalse, equal)
import Expression (Expression(..))
import Expression (betaReduce)

main :: Effect Unit
main = runTest do
    suite "suite" do
        test "some test" do
            assertFalse "" false

standardFunctions =
    suite "Standard functions" do
        test "Identity" do
            let
                identity :: Expression
                identity = Abstraction "a" (Identifier "a")

            equal (Application identity (Identifier "a")) (Identifier "a")
