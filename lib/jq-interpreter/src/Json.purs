module Json
  ( Json(..)
  , Path
  , Target
  , atIndex
  , atKey
  , atPath
  , buildArray
  , buildObject
  , buildString
  , emptyArray
  , emptyObject
  , index
  , key
  , parse
  , parser
  , serialise
  , update
  , values
  )
  where

import Prelude
import Utils.Parsing

import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Array (many)
import Data.Array (drop, fromFoldable, head, index, updateAt, zip) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Map (Map)
import Data.Map (alter, fromFoldable, lookup, keys, values) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String as String
import Data.String.CodeUnits (singleton)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (many1, try)
import Text.Parsing.Parser.String (char, satisfy, string)

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

-- Build
buildArray :: Array Json -> Json
buildArray = JArray

emptyArray :: Json
emptyArray = buildArray []

buildObject :: Array (Tuple Json Json) -> Either String Json
buildObject = traverse keyToString >>> map buildObject_
  where
  keyToString (Tuple k value) = case k of
    JString k -> Right $ Tuple k value
    notAString -> Left $ show notAString <> " cannot be an object key because it is not a string"

buildObject_ :: Array (Tuple String Json) -> Json
buildObject_ = Map.fromFoldable >>> JObject

emptyObject :: Json
emptyObject = buildObject_ []

buildString :: String -> Json
buildString = JString

-- Read
-- TODO - update the Interpreter to just use at instead of the more specific atKey/atIndex
at :: Target -> Json -> Either String Json
at (Key k) json =
  atKey k json
    # note ("Could not find key named " <> k <> " in given json object: " <> show json)
at (Index i) json =
  atIndex i json
    # note ("Could not find item with index" <> show i <> " in given array: " <> show json)

atPath :: Path -> Json -> Either String Json
atPath [] json =
  Right json
atPath targets json =
  let
    defaultTarget = Key "this will not happen as we catch the empty list above"
    target = fromMaybe defaultTarget (Array.head targets)
    remainingTargets = Array.drop 1 targets
  in do
  innerJson <- at target json
  atPath remainingTargets innerJson

atKey :: String -> Json -> Maybe Json
atKey k (JObject object) =
  Map.lookup k object
    # defaultToNull
    >>> Just

atKey _ _ = Nothing

atIndex :: Int -> Json -> Maybe Json
atIndex idx (JArray array) =
  Array.index array idx
    # defaultToNull
    >>> Just

atIndex _ _ = Nothing

defaultToNull :: Maybe Json -> Json
defaultToNull = fromMaybe JNull

values :: Json -> Maybe (Array Json)
values (JArray array) = Just array

values (JObject object) = Just (Map.values object # Array.fromFoldable)

values _ = Nothing

-- Update
type Path
  = Array Target

data Target
  = Key String
  | Index Int

key :: String -> Target
key = Key

index :: Int -> Target
index = Index

update :: Path -> Json -> Json -> Either String Json
update [] newValue _ =
  Right newValue
update targets newValue json =
  let
    defaultTarget = Key "this will not happen as we catch the empty list above"
    target = fromMaybe defaultTarget (Array.head targets)
    remainingTargets = Array.drop 1 targets
  in do
    innerJson <- at target json
    val <- update remainingTargets newValue innerJson
    update_ target val json

update_ :: Target -> Json -> Json -> Either String Json
update_ (Key k) newValue (JObject obj) =
  Right $ JObject (Map.alter (\_ -> Just newValue) k obj)
update_ (Index i) newValue (JArray arr) =
  Array.updateAt i newValue arr
    # map JArray
    # note ("Cannot update. index: " <> show i <> ", array: " <> show arr)
update_ _ _ _ = Left "this json cannot be updated with a target as it is neither an object nor an array"

-- Parse
parse :: String -> Either String Json
parse raw =
  runParser raw parser
    # lmap parseErrorMessage

parser :: Parser String Json
parser =
  fix
    ( \p ->
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
    # map (chrsToString >>> JString)
    # spaced

numberParser :: Parser String Json
numberParser =
  many1 (try digit <|> try dash <|> try dot)
    # map (chrsToString >>> Number.fromString)
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
  JArray <<< Array.fromFoldable <$> (inSquares $ sepByCommas p)
    # spaced

objectParser :: Parser String Json -> Parser String Json
objectParser p = do
  keyValues <- inCurlies $ sepByCommas keyValueParser
  pure $ JObject (Map.fromFoldable keyValues)
  where
  keyValueParser :: Parser String (Tuple String Json)
  keyValueParser = do
    k <- chrsToString <$> quoted (many (satisfy ((/=) '"')))
    _ <- spaced $ (char ':')
    value <- p
    pure $ Tuple k value

-- To String
serialise :: Json -> String
serialise JNull = "null"

serialise (JString s) = wrapWithQuotes s

serialise (JNumber n) = show n

serialise (JBoolean b) = show b

serialise (JArray a) = "[ " <> (String.joinWith ", " (map serialise a)) <> " ]"

serialise (JObject o) = "{ " <> (String.joinWith ", " (map (\(Tuple k v) -> wrapWithQuotes k <> ": " <> serialise v) (toTupleArray o))) <> " }"

wrapWithQuotes :: String -> String
wrapWithQuotes content = "\"" <> content <> "\""

toTupleArray :: forall a b. Map a b -> Array (Tuple a b)
toTupleArray map =
  let
    ks = Array.fromFoldable $ Map.keys map

    vs = Array.fromFoldable $ Map.values map
  in
    Array.zip ks vs

-- Helpers
chrsToString :: forall f. Foldable f => f Char -> String
chrsToString = Foldable.foldMap singleton
