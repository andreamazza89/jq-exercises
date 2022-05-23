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
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple(..), fst, snd)
import Environment (Environment)
import Environment (addFunction, empty) as Env
import Expression (Expression(..), Over(..), Target(..), KeyValuePair, accessByKeyName)
import Json as Json
import Prelude (bind, flip, pure, (#), ($), (>>>))
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage)
import Text.Parsing.Parser.Combinators (many1, optional, try)
import Text.Parsing.Parser.String (eof, satisfy)

type JqParser
  = Parser String ParserOutput

type ParserOutput
  = Tuple Expression Environment

parse :: String -> Either String ParserOutput
parse input =
  runParser input parser
    # lmap parseErrorMessage

parser :: JqParser
parser = do
  exp <-
    fix
      ( \p ->
          expressionParser $ parserConfig (map fst p) allInfixParsers
      )
  _ <- eof
  pure exp

parserConfig :: Parser String Expression -> Array String -> ParserConfig ParserOutput
parserConfig p infixToKeep =
  let
    allPrefix =
      [ withParseEnvironment (parenthesesParser p)
      , withParseEnvironment (objectConstructorParser p)
      , withParseEnvironment (arrayConstructorParser p)
      , withParseEnvironment accessorParser
      , withParseEnvironment identityParser
      , withParseEnvironment literalParser
      ]

    allInfix =
      [ Tuple "update" $ infixLeft "|=" 5 (\(Tuple lexp lenv) (Tuple rexp renv) -> Tuple (Update lexp rexp) Env.empty)
      , Tuple "pipe" $ infixLeft "|" 2 (\(Tuple lexp lenv) (Tuple rexp renv) -> Tuple (Pipe lexp rexp) Env.empty)
      , Tuple "comma" $ infixLeft "," 3 (\(Tuple lexp lenv) (Tuple rexp renv) -> Tuple (Comma lexp rexp) Env.empty)
      ]
  in
    { prefix: allPrefix
    , infixP:
        allInfix
          # Array.filter (fst >>> flip elem infixToKeep)
          # map snd
    }

-- Takes an expression parser and tries to parse an Environment before the Expression, then puts
-- them together into a ParserOutput. The Environment defaults to empty if not present.
withParseEnvironment :: Parser String Expression -> JqParser
withParseEnvironment p = do
  environment <- environmentParser p
  expression <- p
  pure (Tuple expression environment)

environmentParser :: Parser String Expression -> Parser String Environment
environmentParser p = do
  try functionDefinitionParser <|> pure Env.empty
  where
  functionDefinitionParser = do
    _ <- spaced def
    functionName <- ident
    _ <- colon
    expression <- p
    _ <- semiColon
    pure (Env.addFunction ({ name: functionName, arity: 0, body: expression }) Env.empty)

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
      # map fst

parenthesesParser :: Parser String Expression -> Parser String Expression
parenthesesParser = inParentheses

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
objectConstructorParser p =
  sepByCommas (try keyValueParser <|> try shortHandParser)
    # inCurlies
    # map ObjectConstructor
  where
  keyValueParser :: Parser String KeyValuePair
  keyValueParser = do
    key <- p <|> unquotedString
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
