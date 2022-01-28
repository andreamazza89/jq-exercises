module Parser (parse) where

import Prelude (bind, discard, pure, ($), (&&), (/=), (<$>), ($>))
import Control.Lazy (fix)
import Control.Alt ((<|>))
import Data.Array
import Data.Either (Either)
import Data.Foldable as Foldable
import Data.List.Types (NonEmptyList)
import Data.String.CodeUnits as StringStuff
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Expression (Expression(..))
import Text.Parsing.Parser (ParseError, runParser, Parser, fail)
import Text.Parsing.Parser.Combinators (many1, try, optional, chainl)
import Text.Parsing.Parser.String (anyChar, char, satisfy)
import Data.String.CodeUnits (fromCharArray)

parse :: String -> Either ParseError Expression
parse input = runParser input parser

parser :: Parser String Expression
parser =
  try pipeParser
    <|> try foo

foo =
  try selectParser
    <|> try identityParser

pipeParser :: Parser String Expression
pipeParser = do
  chainl foo (char '|' $> Pipe) Identity

selectParser :: Parser String Expression
selectParser = do
  keys <- many1 keyParser
  pure $ Select (keys)

keyParser :: Parser String String
keyParser = do
  _ <- char '.'
  key <- charsToString <$> many1 keyChars
  pure key

keyChars :: Parser String Char
keyChars = do
  satisfy (\c -> c /= '.' && c /= ' ')

charsToString :: NonEmptyList Char -> String
charsToString = Foldable.foldMap StringStuff.singleton

identityParser :: Parser String Expression
identityParser = do
  optional $ char ' '
  _ <- char '.'
  optional $ char ' '
  pure Identity
