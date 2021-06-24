module Lambda.Printer where

import Prelude
import Lambda.Expression (Expression(..))

print :: Expression -> String
print = case _ of
  Variable var -> var
  Application e1 e2 -> "(" <> print e1 <> " " <> print e2 <> ")"
  Abstraction var expression -> "(λ " <> var <> ". " <> print expression <> ")"
