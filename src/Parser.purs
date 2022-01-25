module Parser (parse) where

import Prelude (bind, pure, ($), (&&), (/=), (<$>))

import Data.Either (Either)
import Data.Foldable as Foldable
import Data.List.Types (NonEmptyList)
import Data.String.CodeUnits as StringStuff
import Filter (Filter(..))
import Text.Parsing.Parser (ParseError, runParser, Parser)
import Text.Parsing.Parser.Combinators (choice, many1, try)
import Text.Parsing.Parser.String (char, satisfy)

parse :: String -> Either ParseError Filter
parse input = runParser input parser

parser :: Parser String Filter
parser = do
  choice [ try selectParser, try identityParser ]

selectParser :: Parser String Filter
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
 satisfy (\c -> c /= '.' && c /= ' ' )

charsToString :: NonEmptyList Char -> String
charsToString =
    Foldable.foldMap StringStuff.singleton

identityParser :: Parser String Filter
identityParser = do
  _ <- char '.'
  pure Identity
