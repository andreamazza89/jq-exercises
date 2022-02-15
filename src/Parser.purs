module Parser
  ( identityParser
  , parse
  ) where

import Utils.Parsing

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array.NonEmpty (fromFoldable) as NE
import Data.CodePoint.Unicode (isAlphaNum)
import Data.Either (Either)
import Data.Foldable as Foldable
import Data.Functor (map)
import Data.Int (fromString)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Expression (Expression(..), Over(..), Target(..), KeyValuePair(..))
import Json as Json
import Prelude (bind, pure, (#), ($), (>>>))
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
              [ objectConstructorParser p
              , arrayConstructorParser p
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

allButComma :: Parser String Expression -> Parser String Expression
allButComma p =
  expressionParser
    { prefix:
        [ objectConstructorParser p
        , arrayConstructorParser p
        , accessorParser
        , identityParser
        , literalParser
        ]
    , infixP:
        [ infixLeft "|" 2 Pipe
        ]
    }


literalParser :: Parser String Expression
literalParser = do
  Json.parser
    # map Literal
    # spaced

arrayConstructorParser :: Parser String Expression -> Parser String Expression
arrayConstructorParser p = try emptyArray <|> try arrayWithItems
  where
  emptyArray = do
    _ <- openSquare
    _ <- closeSquare
    pure (ArrayConstructor Nothing)

  arrayWithItems = do
    exp <- inSquares p
    pure $ ArrayConstructor (Just exp)

objectConstructorParser :: Parser String Expression -> Parser String Expression
objectConstructorParser p = do
  try emptyObject <|> objectWithKeyValues
  where
  emptyObject = do
    _ <- openCurly
    _ <- closeCurly
    pure (ObjectConstructor [])

  objectWithKeyValues = do
    keyValues <- inCurlies $ sepByCommas keyValueParser
    pure $ ObjectConstructor (keyValues)

  keyValueParser = do
    key <- p
    _ <- colon
    value <- allButComma p
    pure $ Tuple key value


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
  satisfy (codePointFromChar >>> isAlphaNum)

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
