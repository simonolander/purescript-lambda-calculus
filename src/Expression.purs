module Expression where

import Prelude

type Variable = String

data Expression
    = Identifier Variable
    | Application Expression Expression
    | Abstraction Variable Expression
