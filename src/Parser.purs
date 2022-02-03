module Parser (parse) where

import Prelude (bind, discard, not, pure, ($), ($>), (&&), (/=), (<$>), (<<<))
import Control.Alt ((<|>))
import Data.Int (fromString)
import Data.Array.NonEmpty (fromFoldable, singleton) as NE
import Data.CodePoint.Unicode (isDecDigit, isSpace)
import Data.String.CodePoints (codePointFromChar)
import Data.Either (Either)
import Data.Foldable as Foldable
import Data.List.Types (NonEmptyList)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (singleton)
import Expression (Expression(..), Over(..), Target(..))
import Text.Parsing.Parser (ParseError, runParser, Parser)
import Text.Parsing.Parser.Combinators (between, many1, try, optional, chainl)
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
  keys <- fromMaybe (NE.singleton (Key "redo")) <<< NE.fromFoldable <$> many1 targetParser
  optional $ char ' '
  pure $ Accessor Input keys

targetParser :: Parser String Target
targetParser = do
  try atIndex <|> try wholeArray <|> try atKey

atIndex :: Parser String Target
atIndex = do
  _ <- optional $ char '.'
  index <- between (char '[') (char ']') (fromMaybe 666 <<< fromString <<< charsToString <$> many1 (satisfy (isDecDigit <<< codePointFromChar)))
  pure $ AtIndex index

atKey  :: Parser String Target
atKey = do
  _ <- char '.'
  key <- Key <<< charsToString <$> many1 keyChars
  pure key

keyChars :: Parser String Char
keyChars = do
  satisfy ((\c -> isNotSpace c && isNotIdentity c && isNotSquareBracket c) <<< codePointFromChar)
  where
    isNotIdentity = (/=) (codePointFromChar '.')
    isNotSpace = not <<< isSpace
    isNotSquareBracket c = c /= (codePointFromChar '[') && c /= (codePointFromChar ']')

wholeArray  :: Parser String Target
wholeArray = do
  _ <- optional $ char '.'
  _ <- char '['
  _ <- char ']'
  pure AllItems

charsToString :: NonEmptyList Char -> String
charsToString = Foldable.foldMap singleton

identityParser :: Parser String Expression
identityParser = do
  optional $ char ' '
  _ <- char '.'
  optional $ char ' '
  pure Identity
