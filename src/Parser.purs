module Parser (parse) where

import Utils.Parsing

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (all)
import Data.Array.NonEmpty (fromFoldable) as NE
import Data.CodePoint.Unicode (isSpace)
import Data.Either (Either)
import Data.Foldable as Foldable
import Data.Functor (map)
import Data.Int (fromString)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (singleton)
import Expression (Expression(..), Over(..), Target(..))
import Json as Json
import Prelude (bind, not, pure, (#), ($), (&&), (*>), (/=), (<<<), (>>>))
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (many1, optional, try)
import Text.Parsing.Parser.String (satisfy)

parse :: String -> Either ParseError Expression
parse input = runParser input parser

parser :: Parser String Expression
parser =
  fix
    ( \p ->
        expressionParser
          { prefix:
              [ arrayConstructorParser p
              , accessorParser
              , identityParser
              , literalParser
              ]
          , infixP:
              [ infixLeft "|" 2 Pipe
              , infixLeft "," 3 Comma
              ]
          }
    )

literalParser :: Parser String Expression
literalParser = do
  Json.parser
    # map Literal
    # spaced

accessorParser :: Parser String Expression
accessorParser = do
  targets <- targetsParser
  pure $ Accessor Input targets
  where
  targetsParser =
    many1 targetParser
      # map NE.fromFoldable
      # required
      # spaced

arrayConstructorParser :: Parser String Expression -> Parser String Expression
arrayConstructorParser p = try arrayWithItems
  where
  -- emptyArray = openSquare *> closeSquare *> pure (ArrayConstructor [])

  arrayWithItems = map ArrayConstructor (inSquares p)

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
  many1 keyChars
    # map (charsToString >>> Key)

keyChars :: Parser String Char
keyChars = do
  satisfy (\ch -> all (\test -> test ch) [ isNotIdentity, isNotSpace, isNotSquareBracket ])
  where
  isNotIdentity = (/=) '.'

  isNotSpace = not <<< isSpace <<< codePointFromChar

  isNotSquareBracket c = c /= '[' && c /= ']'

wholeArray :: Parser String Target
wholeArray = do
  _ <- optional dot
  _ <- openSquare
  _ <- closeSquare
  pure Each

identityParser :: Parser String Expression
identityParser = do
  _ <- spaced dot
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
