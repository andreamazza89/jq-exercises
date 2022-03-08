module Interpreter (run) where

import Prelude
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.Maybe (maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Expression (Expression(..), KeyValuePair, toJsonPath, toJsonPaths)
import Json (Json)
import Json (Path, atPath, buildArray, buildObject, emptyArray, emptyObject, update) as Json

type Input
  = Array Json

type Output
  = Array Json

run :: Expression -> Input -> Either String Output
run Identity input = Right input

run (Accessor _ path) input =
  traverse (Json.atPath jsonPath) input
    # map Array.concat
  where
    jsonPath = toJsonPath path

run (Pipe l r) input = run l input >>= run r

run (Literal json) _ = Right [ json ]

run (ArrayConstructor expression) input = maybe emptyArray construct expression
  where
  construct exp =
    run exp input
      # map Json.buildArray
      # map Array.singleton

  emptyArray = Right [ Json.emptyArray ]

run (Comma l r) input = do
  lOutput <- run l input
  rOutput <- run r input
  pure $ lOutput <> rOutput

run (ObjectConstructor []) _ = pure [ Json.emptyObject ]

run (ObjectConstructor keyValuePairs) input = expandKeyValuePairs keyValuePairs input >>= traverse Json.buildObject

run (Update l r) inputs = do
  jsonPaths <- toJsonPaths l
  traverse (runUpdates jsonPaths r) inputs

runUpdates :: Array Json.Path -> Expression -> Json -> Either String Json
runUpdates paths rExp input = foldl runUpdate (pure input) paths
  where
  runUpdate :: Either String Json -> Json.Path -> Either String Json
  runUpdate updatedJson path =
    updatedJson
      >>= Json.update path (runRightExpression >=> pickFirstOutput)

  runRightExpression json = run rExp [ json ]

  pickFirstOutput = Array.head >>> note "The right hand side of an update assignment must return at least one value"

expandKeyValuePairs :: Array (KeyValuePair) -> Input -> Either String (Array (Array (Tuple Json Json)))
expandKeyValuePairs arr input =
  -- Would love to find a way to do this that's easier to grasp, but until then, here's an attempt at explaining:
  -- we start with an array of expressions for all key-values, like so:
  --    `[(K1exp, V1exp), (K2exp, V2exp), ...]`
  -- in the expand phase, we run the interpreter for each expression (using symbols to represent json)
  --    `[([*, ~], [$]), ([^], [&, #]), ...]`
  -- in combineSingleKeyValues, for each pair make all combinations of keys and values:
  --    `[[(*, $), (~, $)], [(^, &), (^, #)], ...]`
  -- and finally we make all possible combinations for all the sets of keyValues:
  --    `[[(*, $), (^, &)], [(*, $), (^, #)], [(~, $), (^, &)], [(~, $), (^, #)] ...]`
  traverse (expand >>> combineSingleKeyValues) arr
    # map combineMultipleKeyValues
  where
  expand (Tuple keyExp valExp) =
    Tuple
      <$> run keyExp input
      <*> run valExp input

  combineSingleKeyValues =
    map
      ( \(Tuple keys values) -> do
          k <- keys
          v <- values
          pure $ Tuple k v
      )

  combineMultipleKeyValues = sequence
