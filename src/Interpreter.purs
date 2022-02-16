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
import Json (atIndex, atKey, buildArray, buildObject2, emptyArray, emptyObject, values) as Json
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

run (ObjectConstructor keyValuePairs) input = do
  kvs <-
    expandKeyValuePairs keyValuePairs input
      # map sequence
  maybe (Left "boom") pure (traverse Json.buildObject2 kvs)

-- stuff <- map Json.buildObject $ run expression input
-- maybe (Left "Fail to build json Object") (Array.singleton >>> pure) stuff
-- [(k, v), (k, v) ...]
-- expandKeyValuePairs
-- [ ([a], [2,3]), ([b,c], [42]) ...]
-- [ [(a,2), (a, 3)], [(b, 42), (c, 42)] ...]]
-- cartesian
-- [ [(a,2), (b, 42)], [(a, 3), (b, 42)], [(a,2), (c,42)]...]]
expandKeyValuePairs :: Array (KeyValuePair) -> Input -> Either String (Array (Array (Tuple Json Json)))
expandKeyValuePairs arr input =
  traverse (expand >>> combine) arr
  where
    expand (Tuple keyExp valExp) =
      Tuple <$> run keyExp input <*> run valExp input
    combine =
      map
        (\(Tuple keys values) -> do
            k <- keys
            v <- values
            pure $ Tuple k v
        )

accessor :: Maybe (Array Json) -> Target -> Maybe (Array Json)
accessor acc (Key k) = acc >>= traverse (Json.atKey k)

accessor acc (AtIndex i) = acc >>= traverse (Json.atIndex i)

accessor acc Each = acc >>= traverse Json.values # map concat

toEither :: Maybe (Array Json) -> Either String (Array Json)
toEither output = case output of
  Just o -> Right o
  Nothing -> Left "Something went wrong in the interpreter (would be nice if this gave more detail)"
