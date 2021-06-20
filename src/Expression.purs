module Expression where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)

type Variable = String

data Expression
    = Identifier Variable
    | Application Expression Expression
    | Abstraction Variable Expression

derive instance genericExpression :: Generic Expression _
instance eqExpression :: Eq Expression where
    eq a b = genericEq a b
instance showExpression :: Show Expression where
    show a = genericShow a

betaReduce :: Expression -> Expression
betaReduce expression =
    case expression of
        Identifier a -> Identifier a
        Application function argument -> function
        Abstraction parameter body -> body
