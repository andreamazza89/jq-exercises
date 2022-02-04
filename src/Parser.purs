module Parser (parse) where

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array.NonEmpty (fromFoldable) as NE
import Data.CodePoint.Unicode (isDecDigit, isSpace)
import Data.Either (Either)
import Data.Foldable as Foldable
import Data.Functor (map)
import Data.Int (fromString)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe, maybe)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (singleton)
import Expression (Expression(..), Over(..), Target(..))
import Prelude (bind, discard, not, pure, (#), ($), ($>), (&&), (/=), (<<<), (>>>))
import Text.Parsing.Parser (ParseError, runParser, Parser, fail)
import Text.Parsing.Parser.Combinators (between, many1, try, optional, chainl)
import Text.Parsing.Parser.String (skipSpaces, char, satisfy)

parse :: String -> Either ParseError Expression
parse input = runParser input parser

parser :: Parser String Expression
parser = fix (\p -> chainl (expressionParser p) (char '|' $> Pipe) Identity)

expressionParser :: Parser String Expression -> Parser String Expression
expressionParser p =
  try (arrayConstructorParser p)
    <|> try accessorParser
    <|> try identityParser

accessorParser :: Parser String Expression
accessorParser = do
  skipSpaces
  targets <- targetsParser
  skipSpaces
  pure $ Accessor Input targets
  where
  targetsParser =
    many1 targetParser
      # map NE.fromFoldable
      # required

arrayConstructorParser :: Parser String Expression -> Parser String Expression
arrayConstructorParser p = do
  _ <- openSquare
  expr <- p
  _ <- closeSquare
  pure (ArrayConstructor expr)

targetParser :: Parser String Target
targetParser = do
  try atIndex <|> try wholeArray <|> try atKey

atIndex :: Parser String Target
atIndex = do
  _ <- optional dot
  index <- inSquares intParser
  pure $ AtIndex index

atKey :: Parser String Target
atKey = do
  _ <- dot
  key <-
    many1 keyChars
      # map (charsToString >>> Key)
  pure key

keyChars :: Parser String Char
keyChars = do
  satisfy ((\c -> isNotSpace c && isNotIdentity c && isNotSquareBracket c) <<< codePointFromChar)
  where
  isNotIdentity = (/=) (codePointFromChar '.')

  isNotSpace = not <<< isSpace

  isNotSquareBracket c = c /= (codePointFromChar '[') && c /= (codePointFromChar ']')

wholeArray :: Parser String Target
wholeArray = do
  _ <- optional dot
  _ <- openSquare
  _ <- closeSquare
  pure Each

identityParser :: Parser String Expression
identityParser = do
  skipSpaces
  _ <- dot
  skipSpaces
  pure Identity

-- Helpers
charsToString :: NonEmptyList Char -> String
charsToString = Foldable.foldMap singleton

charsToInt :: NonEmptyList Char -> Maybe Int
charsToInt = charsToString >>> fromString

intParser :: Parser String Int
intParser =
  many1 digit
    # map charsToInt
    # required

digit :: Parser String Char
digit = satisfy (isDecDigit <<< codePointFromChar)

inSquares :: forall a. Parser String a -> Parser String a
inSquares = between openSquare closeSquare

openSquare :: Parser String Char
openSquare = char '['

closeSquare :: Parser String Char
closeSquare = char ']'

dot :: Parser String Char
dot = char '.'

required :: forall a. Parser String (Maybe a) -> Parser String a
required maybeParser = do
  a <- maybeParser
  maybe (fail "value must exist") pure a
