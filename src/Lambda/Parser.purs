module Lambda.Parser (parse) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Lambda.Expression (Expression(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (chainl1)
import Text.Parsing.Parser.String (char, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser, alphaNum, letter, makeTokenParser)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser (ParseError)
import Data.Either (Either)

clDef :: LanguageDef
clDef =
  LanguageDef
    { commentStart: ""
    , commentEnd: ""
    , commentLine: ""
    , nestedComments: false
    , identStart: letter
    , identLetter: alphaNum
    , opStart: oneOf [ '.', '\\' ]
    , opLetter: oneOf [ '.', '\\' ]
    , reservedNames: [ "lambda", "λ" ]
    , reservedOpNames: [ "." ]
    , caseSensitive: false
    }

tokenParser :: TokenParser
tokenParser = makeTokenParser clDef

dot :: Parser String Char
dot = char '.'

variable :: Parser String Expression
variable = do
  id <- tokenParser.identifier
  pure $ Variable id

application :: Parser String Expression -> Parser String Expression
application parser = do
  fun <- parser
  arg <- parser
  pure $ Application fun arg

lambda :: Parser String String
lambda = tokenParser.symbol "λ"

nonApplication :: Parser String Expression
nonApplication =
  fix \_ ->
    tokenParser.parens term
      <|> abstraction term
      <|> variable

abstraction :: Parser String Expression -> Parser String Expression
abstraction parser = do
  void lambda
  var <- tokenParser.identifier
  void dot
  body <- parser
  pure $ Abstraction var body

term :: Parser String Expression
term = do
  tokenParser.whiteSpace
  chainl1 nonApplication (pure Application)

parse :: String -> Either ParseError Expression
parse input = runParser input term
