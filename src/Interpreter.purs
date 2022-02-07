module Interpreter (run) where

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Expression (Expression(..), Target(..))
import Json (Json)
import Json (atKey, atIndex) as Json
import Prelude ((#), (>>=))

-- 1 open question that will affect the return type:
--  - I think perhaps we should return a Json Array ([Json]), as that's what jq does.
run :: Expression -> Array Json -> Either String (Array Json)
run Identity input = Right input

run (Accessor _ path) input =
  map (\i -> foldl accumulator (Just i) path) input
    # toEither

run (Pipe l r) input = run l input >>= run r

run _ _ = Left "TODO - interpreter support"

accumulator :: Maybe Json -> Target -> Maybe Json
accumulator acc (Key k) = acc >>= Json.atKey k

accumulator acc (AtIndex i) = acc >>= Json.atIndex i

accumulator acc _ = acc

toEither :: forall a. Array (Maybe a) -> Either String (Array a)
toEither maybes = case sequence maybes of
  Just v -> Right v
  Nothing -> Left "Something went wrong in the interpreter (would be nice if this gave more detail)"
