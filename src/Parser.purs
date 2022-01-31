module Parser (parse) where

import Prelude (bind, discard, pure, ($), (&&), (/=), (<$>), ($>), (<<<))
import Control.Alt ((<|>))
import Data.Array (many)
import Data.Either (Either)
import Data.Foldable as Foldable
import Data.List.Types (NonEmptyList)
import Data.String.CodeUnits (singleton)
import Expression (Expression(..), Over(..), Target(..))
import Text.Parsing.Parser (ParseError, runParser, Parser)
import Text.Parsing.Parser.Combinators (many1, try, optional, chainl)
import Text.Parsing.Parser.String (char, satisfy)

parse :: String -> Either ParseError Expression
parse input = runParser input parser

parser :: Parser String Expression
parser = chainl expressionParser (char '|' $> Pipe) Identity

expressionParser :: Parser String Expression
expressionParser =
  try accessorParser
    <|> try identityParser

accessorParser :: Parser String Expression
accessorParser = do
  optional $ char ' '
  keys <- many keyParser
  optional $ char ' '
  pure $ Accessor Input keys

keyParser :: Parser String Target
keyParser = do
  _ <- char '.'
  key <- Key <<< charsToString <$> many1 keyChars
  pure key

keyChars :: Parser String Char
keyChars = do
  satisfy (\c -> c /= '.' && c /= ' ')

charsToString :: NonEmptyList Char -> String
charsToString = Foldable.foldMap singleton

identityParser :: Parser String Expression
identityParser = do
  optional $ char ' '
  _ <- char '.'
  optional $ char ' '
  pure Identity
