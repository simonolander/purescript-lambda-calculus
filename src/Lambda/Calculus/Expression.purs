module Lambda.Calculus.Expression where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

type VarName
  = String

data Expression
  = Variable VarName
  | Application Expression Expression
  | Abstraction VarName Expression

derive instance genericExpression :: Generic Expression _

instance eqExpression :: Eq Expression where
  eq a b = genericEq a b

instance showExpression :: Show Expression where
  show a = genericShow a

isVariable :: Expression -> Boolean
isVariable (Variable _) = true

isVariable _ = false

isApplication :: Expression -> Boolean
isApplication (Application _ _) = true

isApplication _ = false

isAbstraction :: Expression -> Boolean
isAbstraction (Abstraction _ _) = true

isAbstraction _ = false

inUse :: VarName -> Expression -> Boolean
inUse variable expression = case expression of
  Variable i -> i == variable
  Application e1 e2 -> inUse variable e1 || inUse variable e2
  Abstraction v e -> v /= variable && inUse v e

shadowed :: VarName -> Expression -> Boolean
shadowed v e = false

alphaConvert :: VarName -> VarName -> Expression -> Maybe Expression
alphaConvert search replace expression = case expression of
  Variable v
    | v == search -> Just $ Variable replace
    | otherwise -> Just expression
  Abstraction parameter body
    | parameter == search -> case body of
      Abstraction v e -> Nothing
      _ -> do
        newBody <- alphaConvert search replace body
        pure $ Abstraction parameter newBody
    | otherwise -> do
      newBody <- alphaConvert search replace body
      pure $ Abstraction parameter newBody
  _ -> Nothing

betaReduce :: Expression -> Expression
betaReduce expression = case expression of
  Variable _ -> expression
  Application function argument -> function
  Abstraction parameter body -> Abstraction parameter (betaReduce body)
