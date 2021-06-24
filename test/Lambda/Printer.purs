module Test.Lambda.Printer where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Lambda.Parser (parse)
import Lambda.Printer (print)
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

testPrint :: String -> String -> TestSuite
testPrint input expected =
  test (show input <> " -> " <> show expected) do
    case parse input of
      Right expression -> equal expected $ print expression
      Left err -> failure $ show err

main :: Effect Unit
main =
  runTest do
    suite "Printer" do
      testPrint "x" "x"
      testPrint "λx.x" "(λ x. x)"
      testPrint "λx.x y" "(λ x. (x y))"
      testPrint "λf.(λx.f(x x))(λx.f(x x))" "(λ f. ((λ x. (f (x x))) (λ x. (f (x x)))))"
