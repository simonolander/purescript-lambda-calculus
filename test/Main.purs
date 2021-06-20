module Test.Main where

import Prelude
import Effect (Effect)
import Test.Lambda.Expression as Expression
import Test.Lambda.Parser as Parser

main :: Effect Unit
main = do
  Expression.main
  Parser.main
