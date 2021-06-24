module Test.Main where

import Prelude
import Effect (Effect)
import Test.Lambda.Expression as Expression
import Test.Lambda.Parser as Parser
import Test.Lambda.Printer as Printer
import Test.Lambda.Reducer as Reducer

main :: Effect Unit
main = do
  Expression.main
  Parser.main
  Printer.main
  Reducer.main
