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
  , everyItem
  , index
  , key
  , parse
  , parser
  , serialise
  , update
  , values
  ) where

import Prelude
import Utils.Parsing

import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Array (concat, fromFoldable, head, index, singleton, tail, updateAt, zip) as Array
import Data.Array (many)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Map (Map)
import Data.Map (alter, fromFoldable, keys, lookup, values) as Map
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
at :: Target -> Json -> Either String (Array Json)
at (Key k) json =
  atKey k json
    # map Array.singleton
    # note ("Could not find key named " <> k <> " in given json object: " <> show json)

at (Index i) json =
  atIndex i json
    # map Array.singleton
    # note ("Could not find item with index" <> show i <> " in given array: " <> show json)

at EveryItem (JObject obj) =
  Map.values obj
    # Array.fromFoldable
    # pure

at EveryItem (JArray arr) = pure arr

at EveryItem json = Left ("Cannot access every item on json: " <> show json)

atPath :: Path -> Json -> Either String (Array Json)
atPath [] json = Right [ json ]

atPath targets json = do
  innerJson <- at target json
  traverse (atPath remainingTargets) innerJson
    # map Array.concat
  where
  Tuple target remainingTargets = headAndTail targets

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
  | EveryItem

key :: String -> Target
key = Key

index :: Int -> Target
index = Index

everyItem :: Target
everyItem = EveryItem

update :: Path -> (Json -> Either String Json) -> Json -> Either String Json
update [] toNewValue json = toNewValue json

update targets toNewValue json = do
  update_ target (update remainingTargets toNewValue) json
  where
  Tuple target remainingTargets = headAndTail targets

update_ :: Target -> (Json -> Either String Json) -> Json -> Either String Json
update_ (Key k) toNewValue (JObject obj) = do
  newValue <-
    Map.lookup k obj
      # fromMaybe JNull
      # toNewValue
  Map.alter (const (Just newValue)) k obj
    # JObject
    # pure

update_ (Index i) toNewValue (JArray arr) = do
  newValue <-
    Array.index arr i
      # fromMaybe JNull
      # toNewValue
  Array.updateAt i newValue arr
    # map JArray
    # note ("Cannot update. index: " <> show i <> ", array: " <> show arr)

update_ EveryItem toNewValue (JObject obj) = do
  newValues <-
    traverse toNewValue (Map.values obj)
      # map Array.fromFoldable
  Map.fromFoldable (Array.zip ks newValues)
    # JObject
    # pure
  where
    ks = Array.fromFoldable (Map.keys obj)

update_ EveryItem toNewValue (JArray arr) = do
  newValues <- traverse toNewValue arr
  pure (JArray newValues)

update_ _ _ json = Left ("this json cannot be updated with a target as it is neither an object nor an array: " <> show json)

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

-- This function is for ergonomics: purescript does not support pattern matching on an array like (head:tail),
-- so it is to be only used when certain that the array it deals with is not empty.
headAndTail :: Array Target -> Tuple Target (Array Target)
headAndTail arr = Tuple head tail
  where
  head = fromMaybe default (Array.head arr)

  tail = fromMaybe [] (Array.tail arr)

  default = Key "Error - was expecting a non-empty list"
