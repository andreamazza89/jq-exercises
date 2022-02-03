module Parser (parse) where

import Prelude (bind, discard, not, pure, ($), ($>), (*>), (&&), (/=), (<$>), (<<<))
import Control.Alt ((<|>))
import Data.Int (fromString)
import Data.Array.NonEmpty (fromFoldable, singleton) as NE
import Data.CodePoint.Unicode (isDecDigit, isSpace)
import Data.String.CodePoints (codePointFromChar)
import Data.Either (Either)
import Data.Foldable as Foldable
import Data.List.Types (NonEmptyList)
import Data.Maybe (fromMaybe, maybe)
import Data.String.CodeUnits (singleton)
import Expression (Expression(..), Over(..), Target(..))
import Text.Parsing.Parser (ParseError, runParser, Parser, fail)
import Text.Parsing.Parser.Combinators (between, many1, try, optional, chainl)
import Text.Parsing.Parser.String (skipSpaces, char, satisfy)

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
  skipSpaces
  keys <- fromMaybe (NE.singleton (Key "redo")) <<< NE.fromFoldable <$> many1 targetParser
  skipSpaces
  pure $ Accessor Input keys

targetParser :: Parser String Target
targetParser = do
  try atIndex <|> try wholeArray <|> try atKey

atIndex :: Parser String Target
atIndex = do
  _ <- optional dot
  index <- inSquares intParser
  pure $ AtIndex index

atKey  :: Parser String Target
atKey = do
  _ <- dot
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
wholeArray =
  optional dot *> openSquare *> closeSquare *> pure AllItems

charsToString :: NonEmptyList Char -> String
charsToString = Foldable.foldMap singleton

identityParser :: Parser String Expression
identityParser = do
  skipSpaces
  _ <- dot
  skipSpaces
  pure Identity

intParser :: Parser String Int
intParser = do
  n <- fromString <<< charsToString <$> many1 digit
  maybe (fail "failed to parser integer") pure n

digit :: Parser String Char
digit =
    satisfy (isDecDigit <<< codePointFromChar)

inSquares :: forall a . Parser String a -> Parser String a
inSquares = between openSquare closeSquare

openSquare :: Parser String Char
openSquare = char '['

closeSquare :: Parser String Char
closeSquare = char ']'

dot :: Parser String Char
dot = char '.'
