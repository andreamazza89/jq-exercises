module Json (Json(..), at, parser) where

import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable) as Array
import Data.Array (many)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Functor (map)
import Data.Map (Map)
import Data.Map (lookup, fromFoldable) as Map
import Data.Maybe (fromMaybe)
import Data.Number (fromString) as Number
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, bind, pure, show, (#), ($), (*>), (/=), (<$>), (<<<), (>>>))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (between, many1, sepBy, try)
import Text.Parsing.Parser.String (char, satisfy, string)
import Utils.Parsing

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
at key (JObject object) =
  fromMaybe JNull $ Map.lookup key object

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
      <|> try (objectParser p)
  )

nullParser :: Parser String Json
nullParser =
  string "null"
    *> pure JNull
    # spaced

stringParser :: Parser String Json
stringParser =
  quoted (many (satisfy ((/=) '"')))
    # map (charsToString >>> JString)
    # spaced

numberParser :: Parser String Json
numberParser =
  many1 (try digit <|> try dash <|> try dot)
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
  JArray <<< Array.fromFoldable <$> (inSquares $ sepBy p (string ","))
    # spaced

objectParser :: Parser String Json -> Parser String Json
objectParser p = do
  keyValues <- inCurlies $ many keyValueParser
  pure $ JObject (Map.fromFoldable keyValues)
  where
    keyValueParser :: Parser String (Tuple String Json)
    keyValueParser = do
      key <- charsToString <$> quoted (many (satisfy ((/=) '"')))
      _ <- spaced $ (char ':')
      value <- p
      pure $ Tuple key value
  

-- Helpers
charsToString :: forall f. Foldable f => f Char -> String
charsToString = Foldable.foldMap singleton
