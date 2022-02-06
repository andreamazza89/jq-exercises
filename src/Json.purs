module Json (Json(..), at, parser) where

import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable, many)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Functor (map)
import Data.Int (fromString) as Int
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Number (fromString) as Number
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (singleton)
import Prelude (class Eq, class Show, bind, discard, pure, show, (#), ($), (*>), (/=), (<$>), (<<<), (>>>))
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, many1, sepBy, try)
import Text.Parsing.Parser.String (char, satisfy, skipSpaces, string)

data Json
  = JNull
  | JString String
  | JNumber Number
  | JBoolean Boolean
  | JArray (Array Json)
  | JObject (Map String Json)

derive instance equalJson :: Eq Json

instance Show Json where
  show JNull = "null"
  show (JString s) = s
  show (JNumber n) = show n
  show (JBoolean b) = show b
  show (JArray a) = show a
  show (JObject o) = show o

-- Manipulate
at :: String -> Json -> Json
at key (JObject object) = fromMaybe JNull $ lookup key object

at _ _ = JNull

-- Parse
parser :: Parser String Json
parser =
  fix (\p ->
    try nullParser
      <|> try stringParser
      <|> try numberParser
      <|> try booleanParser
      <|> try (arrayParser p)
  )

nullParser :: Parser String Json
nullParser =
  string "null"
    *> pure JNull
    # spaced

stringParser :: Parser String Json
stringParser =
  between (char '"') (char '"') (many (satisfy ((/=) '"')))
    # map (charsToString >>> JString)
    # spaced

numberParser :: Parser String Json
numberParser =
  many1 (try digit <|> try (char '-') <|> try (char '.'))
    # map (charsToString >>> Number.fromString)
    # required
    # spaced
    # map JNumber

booleanParser :: Parser String Json
booleanParser =
  try trueParser <|> try falseParser
    # spaced
  where
    trueParser = do
      _ <- string "true"
      pure $ JBoolean true
    falseParser = do 
      _ <- string "false"
      pure $ JBoolean false

arrayParser :: Parser String Json -> Parser String Json
arrayParser p =
  JArray <<< fromFoldable <$> (inSquares $ sepBy p (string ","))
  # spaced
  

-- These should be extracted to a helper in common with the jq Parser
spaced :: forall a. Parser String a -> Parser String a
spaced p = do
  skipSpaces
  value <- p
  skipSpaces
  pure value


charsToString :: forall f. Foldable f => f Char -> String
charsToString = Foldable.foldMap singleton

digit :: Parser String Char
digit = satisfy (codePointFromChar >>> isDecDigit)

required :: forall a. Parser String (Maybe a) -> Parser String a
required maybeParser = do
  a <- maybeParser
  maybe (fail "value must exist") pure a

inSquares :: forall a. Parser String a -> Parser String a
inSquares = between openSquare closeSquare

openSquare :: Parser String Char
openSquare = spaced $ char '['

closeSquare :: Parser String Char
closeSquare = spaced $ char ']'
