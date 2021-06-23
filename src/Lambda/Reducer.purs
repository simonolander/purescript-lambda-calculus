module Lambda.Reducer where

import Prelude

import Data.Maybe (Maybe(..))
import Lambda.Expression (Expression)

alphaConvert :: String -> String -> Expression -> Maybe Expression
alphaConvert _ _ _ = Nothing

betaReduce :: Expression -> Expression -> Maybe Expression
betaReduce _ _ = Nothing
