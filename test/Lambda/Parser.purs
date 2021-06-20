module Test.Lambda.Parser where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Lambda.Expression (Expression(..))
import Lambda.Parser (term)
import Test.Unit (TestSuite, failure, success, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)
import Text.Parsing.Parser (runParser)

parseTest :: String -> Expression -> TestSuite
parseTest input expected =
  test (show input) do
    case runParser input term of
      Right actual -> equal expected actual
      Left err -> failure $ show err

parseFail :: String -> TestSuite
parseFail input =
  test (show input) do
    case runParser input term of
      Right result -> failure $ "Expected error, got: " <> show result
      Left err -> success

main :: Effect Unit
main =
  runTest do
    suite "Parser" do
      suite "Variables" do
        parseTest "x" (Variable "x")
        parseTest "bar" (Variable "bar")
      suite "Spaces" do
        parseTest " x" (Variable "x")
        parseTest "x " (Variable "x")
        parseTest "  x  " (Variable "x")
        parseTest " λ x . x " (Abstraction "x" (Variable "x"))
        parseTest "  x   x  " (Application (Variable "x") (Variable "x"))
      suite "Parenthesis" do
        parseTest "(x)" (Variable "x")
        parseTest "((x))" (Variable "x")
        parseTest "(((x)))" (Variable "x")
        parseTest "(x x)" (Application (Variable "x") (Variable "x"))
        parseTest "((x x))" (Application (Variable "x") (Variable "x"))
        parseTest "((x)(x))" (Application (Variable "x") (Variable "x"))
        parseTest "((x) (x))" (Application (Variable "x") (Variable "x"))
        parseTest "(λx.x)" (Abstraction "x" (Variable "x"))
        parseTest "(λx.(x))" (Abstraction "x" (Variable "x"))
      suite "Applications" do
        parseTest "x x" (Application (Variable "x") (Variable "x"))
        parseTest "x y z" (Application (Application (Variable "x") (Variable "y")) (Variable "z"))
        parseTest "(x y) z" (Application (Application (Variable "x") (Variable "y")) (Variable "z"))
        parseTest "x (y z)" (Application (Variable "x") (Application (Variable "y") (Variable "z")))
        parseTest "a (b c d)" (Application (Variable "a") (Application (Application (Variable "b") (Variable "c")) (Variable "d")))
        parseTest "a (b c) d" (Application (Application (Variable "a") (Application (Variable "b") (Variable "c"))) (Variable "d"))
        parseTest "(λx.x)y" (Application (Abstraction "x" (Variable "x")) (Variable "y"))
        parseTest "(λx.x) y" (Application (Abstraction "x" (Variable "x")) (Variable "y"))
        parseTest "(λx.x z) λy.w λw.w x y z" (Application (Abstraction "x" (Application (Variable "x") (Variable "z"))) (Abstraction "y" (Application (Variable "w") (Abstraction "w" (Application (Application (Application (Variable "w") (Variable "x")) (Variable "y")) (Variable "z"))))))
      suite "Abstractions" do
        parseTest "λx.x" (Abstraction "x" (Variable "x"))
        parseTest "λfoo.bar" (Abstraction "foo" (Variable "bar"))
        parseTest "λx.x y" (Abstraction "x" (Application (Variable "x") (Variable "y")))
        parseTest "λx.(y z)" (Abstraction "x" (Application (Variable "y") (Variable "z")))
        parseTest "λx.λy.z" (Abstraction "x" (Abstraction "y" (Variable "z")))
        parseTest "λa.λb.λc.d" (Abstraction "a" (Abstraction "b" (Abstraction "c" (Variable "d"))))
        parseTest "λx.x z λy.x y" (Abstraction "x" (Application (Application (Variable "x") (Variable "z")) (Abstraction "y" (Application (Variable "x") (Variable "y")))))
      suite "Syntax error" do
        parseFail ""
        parseFail "."
        parseFail " "
        parseFail "("
        parseFail ")"
        parseFail "(x"
        parseFail "((x)"
        parseFail "λ"
        parseFail "λx"
        parseFail "λx."
        parseFail "λ.x"
        parseFail "λλ.x"
        parseFail "λ λ.x"

--        parseFail "λλx.x"
--        parseFail "λ λx.x"
