module Parser
  ( identityParser
  , parse
  ) where

import Utils.Parsing
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (elem)
import Data.Array as Array
import Data.Array.NonEmpty (fromFoldable) as NE
import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode (isAlphaNum)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple(..), fst, snd)
import Environment (Environment)
import Environment (empty, fromFunction) as Env
import Expression (Expression(..), KeyValuePair, Over(..), Target(..), accessByKeyName)
import Json as Json
import Prelude (bind, flip, pure, (#), ($), (>>>))
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage)
import Text.Parsing.Parser.Combinators (many1, optional, try)
import Text.Parsing.Parser.String (eof, satisfy)

type JqParser
  = Parser String ParserOutput

type ParserOutput
  = Tuple Expression (Environment Expression)

parse :: String -> Either String ParserOutput
parse input =
  runParser input parser
    # lmap parseErrorMessage

parser :: JqParser
parser = do
  parserOutput <-
    fix
      ( \p -> do
          env <- environmentParser (map fst p)
          expression <- expressionParser $ parserConfig (map fst p) allInfixParsers
          pure (Tuple expression env)
      )
  _ <- eof
  pure parserOutput

parserConfig :: Parser String Expression -> Array String -> ParserConfig Expression
parserConfig p infixToKeep =
  let
    allPrefix =
      [ parenthesesParser p
      , objectConstructorParser p
      , arrayConstructorParser p
      , functionApplicationParser
      , accessorParser
      , identityParser
      , literalParser
      ]

    allInfix =
      [ Tuple "update" $ infixLeft "|=" 5 Update
      , Tuple "pipe" $ infixLeft "|" 2 Pipe
      , Tuple "comma" $ infixLeft "," 3 Comma
      ]
  in
    { prefix: allPrefix
    , infixP:
        allInfix
          # Array.filter (fst >>> flip elem infixToKeep)
          # map snd
    }

environmentParser :: Parser String Expression -> Parser String (Environment Expression)
environmentParser p = do
  try (map fold functionsParser) <|> pure Env.empty
  where
  functionsParser = many1 functionDefinitionParser
  functionDefinitionParser = do
    _ <- spaced def
    functionName <- ident
    _ <- spaced colon
    expression <- p
    _ <- spaced semiColon
    pure $ Env.fromFunction { name: functionName, arity: 0, body: expression }

allInfixParsers :: Array String
allInfixParsers =
  [ "update"
  , "pipe"
  , "comma"
  ]

objectValueParser :: Parser String Expression -> Parser String Expression
objectValueParser p =
  let
    allInfixButComma = Array.delete "comma" allInfixParsers
  -- When parsing an object constructor, we need to 'disable' the comma parser while
  -- parsing the right hand side of a `<key>: <value>` pair.
  -- This is because otherwise given something like `"a": 42, "b": 33`, our parser would
  -- consume the comma operator, like `("a" |key) : (42 , "b" |value)` and then blow up.
  in
    expressionParser (parserConfig p allInfixButComma)

parenthesesParser :: Parser String Expression -> Parser String Expression
parenthesesParser = inParentheses

literalParser :: Parser String Expression
literalParser = do
  Json.parser
    # map Literal
    # spaced

functionApplicationParser :: Parser String Expression
functionApplicationParser = do
  map Apply ident

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
objectConstructorParser p =
  sepByCommas (try keyValueParser <|> try shortHandParser)
    # inCurlies
    # map ObjectConstructor
  where
  keyValueParser :: Parser String KeyValuePair
  keyValueParser = do
    key <- unquotedString <|> p
    _ <- colon
    value <- objectValueParser p
    pure $ Tuple key value

  shortHandParser :: Parser String KeyValuePair
  shortHandParser = do
    identifier <- ident
    pure $ Tuple (toStringLiteral identifier) (accessByKeyName identifier)

  unquotedString :: Parser String Expression
  unquotedString = map toStringLiteral ident

  toStringLiteral :: String -> Expression
  toStringLiteral = Json.buildString >>> Literal

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
