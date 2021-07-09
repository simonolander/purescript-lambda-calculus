module Test.Lambda.Calculus.Reducer where

import Prelude
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.List.Lazy (drop, take)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Lambda.Calculus.Expression (Expression)
import Lambda.Calculus.Parser (parse)
import Lambda.Calculus.Reducer (allVariables, betaReduce, etaReduce, freeVars, isFree, newVar, nextVar, redex, reduce, reduceStep)
import Test.Unit (Test, TestSuite, failure, suite, test, timeout)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

parseThen :: String -> (Expression -> Test) -> Test
parseThen input f = case parse input of
  Right expr -> f expr
  Left err -> failure $ "Failed parsing " <> show input <> ": " <> show err

main :: Effect Unit
main =
  runTest do
    suite "Reducer" do
      testVariables
      testRedex
      testReduceStep
      testEtaReduction
      testBetaReduction
      testReduction

testVariables :: TestSuite
testVariables =
  suite "Free variables" do
    suite "Free" do
      let
        t :: String -> String -> TestSuite
        t var expr =
          test (var <> " in " <> show expr)
            $ parseThen expr \expr' -> equal true (isFree var expr')
      t "x" "x"
      t "x" "x y"
      t "x" "λy.x"
      t "x" "(λy.y) x"
      t "x" "(λy.y) (λz.x)"
      t "x" "λy.λy.λy.λy.x"
    suite "Not free" do
      let
        t :: String -> String -> TestSuite
        t var expr =
          test (var <> " in " <> show expr)
            $ parseThen expr \expr' -> equal false (isFree var expr')
      t "x" "y"
      t "x" "y y"
      t "x" "λy.y"
      t "x" "λx.x"
      t "x" "(λy.y) z"
      t "x" "(λx.x) z"
      t "x" "(λx.x) (λx.x)"
      t "x" "λy.λy.λx.λy.x"
    suite "All free variables" do
      let
        t :: String -> String -> TestSuite
        t expr expected =
          test (expr <> ": " <> expected)
            $ parseThen expr \expr' ->
                equal
                  (Set.filter (not <<< String.null) $ Set.fromFoldable $ String.split (Pattern " ") expected)
                  (freeVars expr')
      t "x" "x"
      t "x y" "x y"
      t "λx.x" ""
      t "λx.λy.z" "z"
      t "λx.λy.x y" ""
      t "(λx.x) x" "x"
      t "(λx.a) c" "a c"
    suite "New variable" do
      let
        t :: String -> String -> TestSuite
        t vars expected =
          test (vars <> " -> " <> expected)
            $ vars
            # String.split (Pattern " ")
            # Set.fromFoldable
            # Set.filter (not <<< String.null)
            # newVar
            # equal expected
            # timeout 100
      t "x" "a"
      t "a" "b"
      t "a b" "c"
      t "a c" "b"
      t "a b c d e f g h i j k l m n o p q r s t u v w x y z" "aa"
    suite "Next variable" do
      let
        t :: String -> String -> TestSuite
        t var expected =
          test (var <> " -> " <> expected)
            $ equal expected (nextVar var)
      t "" "a"
      t "a" "b"
      t "b" "c"
      t "z" "aa"
      t "aa" "ab"
      t "az" "ba"
      t "az" "ba"
      t "ba" "bb"
      t "zzz" "aaaa"
    test "All variables" do
      equal [ "a", "b", "c" ] (fromFoldable $ take 3 allVariables)
      equal [ "aaa", "aab", "aac" ] (fromFoldable $ take 3 $ drop (26 * 27) allVariables)

testRedex :: TestSuite
testRedex =
  suite "Reducible expressions" do
    suite "Redex" do
      let
        t :: String -> TestSuite
        t expr =
          test expr
            $ parseThen expr \expr' -> equal true (redex expr')
      t "(λx.x) x"
      t "(λx.x) x y"
      t "x ((λx.x) x)"
      t "(λx.x) (λx.x)"
      t "((λx.x) x) (λx.x)"
      t "λx.f x"
      t "λd.a b c d"
      t "λa.(λb.a) a"
      t "λa.(λb.b) a"
      t "λa.(λb.c) a"
      t "(λa.λb.a b)"
    suite "Not redex" do
      let
        t :: String -> TestSuite
        t expr =
          test expr
            $ parseThen expr \expr' -> equal false (redex expr')
      t "x"
      t "x x"
      t "x y"
      t "λx.x"
      t "x λx.x"
      t "x (λx.x) x"
      t "λx.λx.λx.x"
      t "λx.x f"
      t "λc.a b c d"
      t "λa.λb.a"
      t "λa.λb.b a"

testReduceStep :: TestSuite
testReduceStep =
  suite "Reduce single step" do
    let
      t :: String -> String -> TestSuite
      t input expected =
        test (input <> " -> " <> expected) do
          parseThen input \input' -> do
            parseThen expected \expected' -> do
              equal expected' (reduceStep input')
    t "x" "x"
    t "λx.f x" "f"
    t "λd.a b c d" "a b c"
    t "λa.(λb.b) a" "λb.b"
    t "x x" "x x"
    t "x y" "x y"
    t "λx.x" "λx.x"
    t "x λx.x" "x λx.x"
    t "x (λx.x) x" "x (λx.x) x"
    t "λx.λx.λx.x" "λx.λx.λx.x"
    t "λx.x f" "λx.x f"
    t "λc.a b c d" "λc.a b c d"
    t "λa.λb.a" "λa.λb.a"
    t "(λx.x) y" "y"
    t "(λx.x) y z" "y z"
    t "(λx.x) (y z)" "y z"
    t "(λx.x) (λx.x)" "λx.x"
    t "y ((λx.x) z)" "y z"
    t "((λx.x) y) λx.x" "y λx.x"
    t "λa.(λb.c) a" "λb.c"
    t "λa.λb.a a" "λa.λb.a a"
    t "λa.(λb.a) a" "λa.a"
    t "(λa.λb.a) b" "λc.b"
    t "(λa.λb.b) b" "λb.b"
    t "(λa.λb.a b) b" "(λa.a) b"
    t "(λa.λb.b a) b" "λc.c b"
    t "(λa.λb.a) a" "λb.a"
    t "(λx.x) x" "x"
    t "(λx.x) x y" "x y"
    t "x ((λx.x) x)" "x x"
    t "((λx.x) x) (λx.x)" "x λx.x"
    t "x (λx.x) x" "x (λx.x) x"
    t "(λx.x x) (λx.x x)" "(λx.x x) (λx.x x)"
    t "(λx.x x x) λx.x" "(λx.x) (λx.x) λx.x"

testEtaReduction :: TestSuite
testEtaReduction =
  suite "η-reduction" do
    let
      pos :: String -> String -> TestSuite
      pos input expected =
        test (input <> " -> " <> expected) do
          parseThen input \input' -> do
            parseThen expected \expected' -> do
              equal (Just expected') (etaReduce input')

      neg :: String -> TestSuite
      neg input =
        test ("cannot η-reduce " <> input) do
          parseThen input \input' -> do
            equal Nothing (etaReduce input')
    pos "λx.f x" "f"
    pos "λx.a b c x" "a b c"
    pos "λx.(λy.y) x" "λy.y"
    neg "x"
    neg "x y"
    neg "(λy.y) (λy.y)"
    neg "λx.x x"
    neg "λx.a x x"
    neg "λx.λy.y x"

testBetaReduction :: TestSuite
testBetaReduction =
  suite "β-reduction" do
    let
      pos :: String -> String -> TestSuite
      pos input expected =
        test (input <> " -> " <> expected) do
          parseThen input \input' -> do
            parseThen expected \expected' -> do
              equal (Just expected') (betaReduce input')

      neg :: String -> TestSuite
      neg input =
        test ("cannot β-reduce " <> input) do
          parseThen input \input' -> do
            equal Nothing (betaReduce input')
    neg "x"
    neg "x y"
    neg "x y z"
    neg "λx.f x"
    neg "λx.x"
    neg "a λx.x"
    neg "λx.(λx.x) x"
    pos "(λx.x) x" "x"
    pos "(λx.x) a" "a"
    pos "(λx.c) x" "c"
    pos "(λx.x x) y" "y y"
    pos "(λx.λy.x) c" "λy.c"
    pos "(λx.λy.x) (a b)" "λy.a b"
    pos "(λx.λy.x) y" "λa.y"
    pos "(λx.λy.x) (a y)" "λb.a y"
    pos "(λa.λb.a) (a b c)" "λd.a b c"
    pos "(λx.x x) (λx.x x)" "(λx.x x) (λx.x x)"
    pos "(λx.x) (λx.x)" "λx.x"

testReduction :: TestSuite
testReduction =
  suite "Normal order reduction" do
    let
      t :: String -> Boolean -> String -> TestSuite
      t input expectedComplete expectedResult =
        test (input <> " -> " <> expectedResult) do
          parseThen input \input' -> do
            parseThen expectedResult \expectedResult' -> do
              let
                reduction = reduce 10 input'
              equal expectedComplete reduction.complete
              equal expectedResult' reduction.result
    t "x" true "x"
    t "x y" true "x y"
    t "x y z" true "x y z"
    t "λx.f x" true "f"
    t "λx.x" true "λx.x"
    t "a λx.x" true "a λx.x"
    t "(λx.x) x" true "x"
    t "(λx.x) a" true "a"
    t "(λx.c) x" true "c"
    t "(λx.x x) y" true "y y"
    t "(λx.λy.x) c" true "λy.c"
    t "(λx.λy.x) (a b)" true "λy.a b"
    t "(λx.λy.x) y" true "λa.y"
    t "(λx.λy.x) (a y)" true "λb.a y"
    t "(λa.λb.a) (a b c)" true "λd.a b c"
    t "(λx.x) (λx.x)" true "λx.x"
    t "(λx.x x) (λx.x x)" false "(λx.x x) (λx.x x)"
    t "x ((λx.x x) (λx.x x))" false "x ((λx.x x) (λx.x x))"
    t "(λx.y) ((λx.x x) (λx.x x))" true "y"
    t "(λx.λy.x) (λx.x) ((λx.x x) (λx.x x))" true "λx.x"
    t "λx.λy.x" true "λx.λy.x" -- True
    t "λx.λy.y" true "λx.λy.y" -- False
    t "(λp.λq.p q p) (λx.λy.x) (λx.λy.x)" true "λx.λy.x" -- And True True == True
    t "(λp.λq.p q p) (λx.λy.x) (λx.λy.y)" true "λx.λy.y" -- And True False == False
    t "(λp.λq.p q p) (λx.λy.y) (λx.λy.x)" true "λx.λy.y" -- And False True == False
    t "(λp.λq.p q p) (λx.λy.y) (λx.λy.y)" true "λx.λy.y" -- And False False == False
    t "(λp.λq.p p q) (λx.λy.x) (λx.λy.x)" true "λx.λy.x" -- Or True True == True
    t "(λp.λq.p p q) (λx.λy.x) (λx.λy.y)" true "λx.λy.x" -- Or True False == True
    t "(λp.λq.p p q) (λx.λy.y) (λx.λy.x)" true "λx.λy.x" -- Or False True == True
    t "(λp.λq.p p q) (λx.λy.y) (λx.λy.y)" true "λx.λy.y" -- Or False False == False
    t "(λp.p (λx.λy.y) (λx.λy.x)) (λx.λy.x)" true "λx.λy.y" -- Not True == False
    t "(λp.p (λx.λy.y) (λx.λy.x)) (λx.λy.y)" true "λx.λy.x" -- Not False == True
