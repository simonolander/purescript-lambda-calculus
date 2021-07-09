module Test.Main where

import Prelude
import Effect (Effect)
import Test.Lambda.Calculus.Expression as Expression
import Test.Lambda.Calculus.Parser as Parser
import Test.Lambda.Calculus.Printer as Printer
import Test.Lambda.Calculus.Reducer as Reducer

main :: Effect Unit
main = do
  Expression.main
  Parser.main
  Printer.main
  Reducer.main
