module Lambda.Reducer where

import Prelude
import Data.Array as Array
import Data.Enum (succ)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (CodePoint)
import Data.String as String
import Lambda.Expression (Expression(..), VarName, isAbstraction)

--| A list of all variables in order
allVariables :: LL.List String
allVariables = LL.iterate nextVar "a"

--| Returns all free variables in `expr`
freeVars :: Expression -> Set String
freeVars expr = case expr of
  Variable v -> Set.singleton v
  Abstraction param body -> Set.delete param $ freeVars body
  Application e1 e2 -> Set.union (freeVars e1) (freeVars e2)

--| Returns the next var that's not in `vars`
newVar :: Set String -> String
newVar vars = newVar' "a"
  where
  newVar' :: String -> String
  newVar' var =
    if Set.member var vars then
      newVar' (nextVar var)
    else
      var

--| Returns next variable after `var`
nextVar :: String -> String
nextVar var =
  String.toCodePointArray var
    # Array.reverse
    # List.fromFoldable
    # next'
    # Array.fromFoldable
    # Array.reverse
    # String.fromCodePointArray
  where
  next' :: List CodePoint -> List CodePoint
  next' v =
    let
      a :: CodePoint
      a = String.codePointFromChar 'a'

      z :: CodePoint
      z = String.codePointFromChar 'z'
    in
      case v of
        Cons head tail ->
          if head >= z then
            a : next' tail
          else case succ head of
            Just head' -> head' : tail
            Nothing -> z : tail
        Nil -> List.singleton a

--| Returns true if there are any steps that can be taken
--| to reduce `expr` to a potentially smaller expression
redex :: Expression -> Boolean
redex expr = case expr of
  Variable _ -> false
  Application e1 e2
    | isAbstraction e1 -> true
    | otherwise -> redex e1 || redex e2
  Abstraction param body -> redex body || etaReducible param body
  where
  etaReducible :: String -> Expression -> Boolean
  etaReducible param body = case body of
    Application e1 e2 -> e2 == Variable param && not isFree param e1
    _ -> false

--| Substitutes `from` with `to` in `expr`, taking care to not
--| shadow any free variables in `to` during the process
safeSubstitute :: String -> Expression -> Expression -> Expression
safeSubstitute from to expr = case expr of
  Variable v
    | v == from -> to
    | otherwise -> expr
  Application e1 e2 -> Application (safeSubstitute from to e1) (safeSubstitute from to e2)
  Abstraction param body
    | isFree param to ->
      let
        freeTo = freeVars to

        freeBody = freeVars body

        free = Set.union freeTo freeBody

        replacement = newVar free
      in
        safeSubstitute param (Variable replacement) body
          # safeSubstitute from to
          # Abstraction replacement
    | otherwise -> Abstraction param (safeSubstitute from to body)

reduceStep :: Expression -> Expression
reduceStep expr = case expr of
  Variable _ -> expr
  Application e1 e2 -> reduceStepApplication e1 e2
  Abstraction param body -> reduceStepAbstraction param body
  where
  reduceStepApplication e1 e2 =
    if redex e1 then
      Application (reduceStep e1) e2
    else if redex e2 then
      Application e1 (reduceStep e2)
    else case e1 of
      Abstraction param body ->
        if isFree param body then
          safeSubstitute param e2 body
        else
          body
      _ -> Application e1 e2

  reduceStepAbstraction param body = case body of
    Application e1 e2
      | e2 == Variable param && not isFree param e1 -> e1
      | otherwise -> Abstraction param (reduceStep body)
    _ ->
      if redex body then
        Abstraction param (reduceStep body)
      else
        Abstraction param body

--| Returns true if `var` is one of the free variables of `expr`.
isFree :: VarName -> Expression -> Boolean
isFree var expr = case expr of
  Variable v -> v == var
  Application e1 e2 -> isFree var e1 || isFree var e2
  Abstraction parameter body -> parameter /= var && isFree var body

--| Replaces all occurences of `from` with `to` in `expression.
--| The caller is assumed to have checked that such a substitution
--| is allowed.
substitute :: String -> Expression -> Expression -> Expression
substitute from to expression = case expression of
  Variable var -> if var == from then to else expression
  Application e1 e2 -> Application (substitute from to e1) (substitute from to e2)
  Abstraction parameter body -> if parameter == from then expression else Abstraction parameter (substitute from to body)

data Step
  = EtaReduction Expression Expression
  | BetaReduction Expression Expression

type Reduction
  = { result :: Expression
    , complete :: Boolean
    , steps :: List Step
    }

--| Performs an η-reduction on the provided expression, if possible.
--| An η-reduction is possible for abstractions of the form Lx. f x, iff
--| x is not free in f. The result is f.
--|
--| Returns the reduced expression if successful, or nothing if unsuccessful.
etaReduce :: Expression -> Maybe Expression
etaReduce expr = case expr of
  Abstraction param (Application e1 e2)
    | e2 /= Variable param -> Nothing
    | isFree param e1 -> Nothing
    | otherwise -> Just e1
  _ -> Nothing

--| Performs an α-conversion on the given abstraction, taking care to perform additional α-conversions
--| down the line if needed to prevent unintended shadowing
alphaConvert :: String -> Expression -> Expression
alphaConvert newParameter expr = case expr of
  Abstraction oldParameter body -> Abstraction newParameter (betaSubstitute oldParameter (Variable newParameter) body)
  _ -> expr

--| Substitutes all occurrences of `search` in `expr` with `replace`. Takes care of performing α-conversions
--| along the way if the substitution would shadow variables in `replace`.
betaSubstitute :: String -> Expression -> Expression -> Expression
betaSubstitute search replace expr = case expr of
  Variable v ->
    if v == search then
      replace
    else
      expr
  Application e1 e2 -> Application (betaSubstitute search replace e1) (betaSubstitute search replace e2)
  Abstraction param body ->
    if param == search then
      expr
    else if isFree param replace then
      let
        freeVarsInReplace = freeVars replace

        freeVarsInBody = freeVars body

        newParameter = newVar (Set.union freeVarsInReplace freeVarsInBody)
      in
        alphaConvert newParameter expr
          # betaSubstitute search replace
    else
      Abstraction param (betaSubstitute search replace body)

--| Performs a β-reduction on `expr` attempting to apply an argument to an abstraction.
--| β-reductions are only possible for expressions of the form (Application (Abstraction _ _) _).
--| Even in cases where β-reductions are possible, it's not guaranteed that the result will be
--| smaller than the input.
--|
--| Returns the reduced expression if the reduction was successful, otherwise nothing.
betaReduce :: Expression -> Maybe Expression
betaReduce expr = case expr of
  Application (Abstraction param body) e2 -> Just (betaSubstitute param e2 body)
  _ -> Nothing

--| Reduced the given expression according to the normal order strategy, i.e.
--| whenever possible the arguments are substituted into the body of an abstraction
--| before the arguments are reduced.
reduce :: Int -> Expression -> Reduction
reduce depth expr =
  if depth <= 0 then
    { result: expr
    , complete: false
    , steps: Nil
    }
  else case etaReduce expr of
    Just etaReduced ->
      let
        reduction = reduce depth etaReduced
      in
        reduction { steps = (EtaReduction expr etaReduced) : reduction.steps }
    Nothing -> case betaReduce expr of
      Just betaReduced ->
        let
          reduction = reduce (depth - 1) betaReduced
        in
          reduction { steps = (BetaReduction expr betaReduced) : reduction.steps }
      Nothing -> case expr of
        Variable _ ->
          { result: expr
          , complete: true
          , steps: Nil
          }
        Application e1 e2 ->
          let
            e1Reduction = reduce depth e1
          in
            if e1Reduction.complete && e1Reduction.result /= e1 then
              let
                reduction = reduce depth (Application e1Reduction.result e2)
              in
                reduction { steps = e1Reduction.steps <> reduction.steps }
            else
              let
                e2Reduction = reduce depth e2
              in
                { result: Application e1Reduction.result e2Reduction.result
                , complete: e1Reduction.complete && e2Reduction.complete
                , steps: e1Reduction.steps <> e2Reduction.steps
                }
        Abstraction param body ->
          let
            bodyReduction = reduce depth body
          in
            bodyReduction { result = Abstraction param bodyReduction.result }
