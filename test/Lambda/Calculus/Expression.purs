module Test.Lambda.Calculus.Expression where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Lambda.Calculus.Expression (Expression(..), alphaConvert, betaReduce)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

a :: Expression
a = Variable "a"

b :: Expression
b = Variable "b"

c :: Expression
c = Variable "c"

id :: Expression
id = Abstraction "a" a

main :: Effect Unit
main =
  runTest do
    suite "Expression" do
      testAlphaConversion
      testBetaReduction

testAlphaConversion :: TestSuite
testAlphaConversion =
  suite "α-conversion" do
    suite "Identifiers" do
      test "a[a := a] == a" do
        equal (Just a) (alphaConvert "a" "a" a)
      test "a[a := b] == b" do
        equal (Just b) (alphaConvert "a" "b" a)
      test "a[b := c] == a" do
        equal (Just a) (alphaConvert "b" "c" a)

--        suite "Abstractions" dom
--            test "λa.a[a := a] == λa.a" do
--                equal (Just id) (alphaConvert "a" "a" id)
--            test "λa.a[a := b] == λb.b" do
--                equal (Just $ Abstraction "b" b) (alphaConvert "a" "b" id)
--            test "λa.a[b := c] == λa.a" do
--                equal (Just $ Abstraction "a" a) (alphaConvert "b" "c" id)
--            test "λab.c[a := b] == λbb.c" do
--                let
--                    expected = Just $ Abstraction "b" $ Abstraction "b" c
--                    actual = alphaConvert "a" "b" $ Abstraction "a" $ Abstraction "b" c
--                equal expected actual
--            test "λab.a[a := c] == λcb.c" do
--                let
--                    expected = Just $ Abstraction "c" $ Abstraction "b" c
--                    actual = alphaConvert "a" "c" $ Abstraction "a" $ Abstraction "b" a
--                equal expected actual
--            test "λaa.a[a := b] == λba.a" do
--                let
--                    expected = Just $ Abstraction "b" $ Abstraction "a" a
--                    actual = alphaConvert "a" "b" $ Abstraction "a" $ Abstraction "a" a
--                equal expected actual
--            test "λab.a[a := b] fails" do
--                let
--                    expected = Nothing
--                    actual = alphaConvert "a" "b" $ Abstraction "a" $ Abstraction "b" a
--                equal expected actual
testBetaReduction :: TestSuite
testBetaReduction =
  suite "β-reduction" do
    suite "Identifiers" do
      test "β-reduce 'a' == 'a'" do
        equal (betaReduce a) a
      test "β-reduce 'b' == 'b'" do
        equal (betaReduce b) b
      test "β-reduce 'c' == 'c'" do
        equal (betaReduce c) c
    suite "Abstractions" do
      test "β-reduce id == id" do
        equal (betaReduce id) id
