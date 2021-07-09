module Lambda.Calculus.Parser (parse) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (concat)
import Data.Either (Either)
import Lambda.Calculus.Expression (Expression(..))
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (chainl1)
import Text.Parsing.Parser.String (char, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser, makeTokenParser)

lowercaseChar :: Array Char
lowercaseChar = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'å', 'ä', 'ö' ]

uppercaseChar :: Array Char
uppercaseChar = [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'Å', 'Ä', 'Ö' ]

digitChar :: Array Char
digitChar = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

languageDef :: LanguageDef
languageDef =
  LanguageDef
    { commentStart: ""
    , commentEnd: ""
    , commentLine: ""
    , nestedComments: false
    , identStart: oneOf $ concat [lowercaseChar, uppercaseChar]
    , identLetter: oneOf $ concat [ lowercaseChar, digitChar, uppercaseChar ]
    , opStart: oneOf [ '.', '^', '\\']
    , opLetter: oneOf [ '.', '^', '\\']
    , reservedNames: []
    , reservedOpNames: [ "." ]
    , caseSensitive: false
    }

tokenParser :: TokenParser
tokenParser = makeTokenParser languageDef

dot :: Parser String Char
dot = char '.'

variable :: Parser String Expression
variable = do
  id <- tokenParser.identifier
  pure $ Variable id

lambda :: Parser String String
lambda = tokenParser.symbol "λ" <|> tokenParser.symbol "\\"

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
