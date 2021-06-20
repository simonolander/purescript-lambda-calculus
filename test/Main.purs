module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Expression as Expression

main :: Effect Unit
main = do
  Expression.main
