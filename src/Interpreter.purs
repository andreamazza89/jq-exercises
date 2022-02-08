module Interpreter (run) where

import Data.Array (concat)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Expression (Expression(..), Target(..))
import Json (Json)
import Json (atIndex, atKey, buildArray, values) as Json
import Prelude (flip, (#), (>>=), (>>>))

run :: Expression -> Array Json -> Either String (Array Json)
run Identity input = Right input

run (Accessor _ path) input =
  foldl accumulator (Just input) path
    # toEither

run (Pipe l r) input = run l input >>= run r

run (Literal json) _ = Right [ json ]

run (ArrayConstructor expressions) input =
  traverse (flip run input) expressions
    # map (concat >>> Json.buildArray >>> Array.singleton)

accumulator :: Maybe (Array Json) -> Target -> Maybe (Array Json)
accumulator acc (Key k) = acc >>= traverse (Json.atKey k)

accumulator acc (AtIndex i) = acc >>= traverse (Json.atIndex i)

accumulator acc Each = acc >>= traverse Json.values # map concat

toEither :: Maybe (Array Json) -> Either String (Array Json)
toEither output = case output of
  Just o -> Right o
  Nothing -> Left "Something went wrong in the interpreter (would be nice if this gave more detail)"
