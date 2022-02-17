module Interpreter (run) where

import Data.Array (concat)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Expression (Expression(..), Target(..), KeyValuePair)
import Json (Json)
import Json (atIndex, atKey, buildArray, buildObject, emptyArray, emptyObject, values) as Json
import Prelude (bind, pure, (#), ($), (<$>), (<*>), (<>), (>>=), (>>>))

type Input
  = Array Json

type Output
  = Array Json

run :: Expression -> Input -> Either String Output
run Identity input = Right input

run (Accessor _ path) input =
  foldl accessor (Just input) path
    # toEither

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
  lExp <- run l input
  rExp <- run r input
  pure $ lExp <> rExp

run (ObjectConstructor []) _ = pure [ Json.emptyObject ]
run (ObjectConstructor keyValuePairs) input =
  expandKeyValuePairs keyValuePairs input >>= traverse Json.buildObject

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

accessor :: Maybe (Array Json) -> Target -> Maybe (Array Json)
accessor acc (Key k) = acc >>= traverse (Json.atKey k)

accessor acc (AtIndex i) = acc >>= traverse (Json.atIndex i)

accessor acc Each = acc >>= traverse Json.values # map concat

toEither :: Maybe (Array Json) -> Either String (Array Json)
toEither output = case output of
  Just o -> Right o
  Nothing -> Left "Something went wrong in the interpreter (would be nice if this gave more detail)"
