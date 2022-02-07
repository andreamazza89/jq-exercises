module Interpreter (run) where

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Expression (Expression(..), Target(..))
import Json (Json)
import Json (atKey, atIndex) as Json
import Prelude ((#), (>>=))

-- 2 open questions that will affect the return type:
--   1 - Does this operation ever fail, or just default to Null? As in, given a valid expression and Json, can this ever
--       fail, or do we just return a JNull?
--  2 - I think perhaps we should return a Json Array ([Json]), as that's what jq does.
run :: Expression -> Json -> Either String Json
run (Accessor _ path) input =
  foldl accumulator (Just input) path
    # toEither

run _ input = Right input

accumulator :: Maybe Json -> Target -> Maybe Json
accumulator acc (Key k) = acc >>= Json.atKey k

accumulator acc (AtIndex i) = acc >>= Json.atIndex i

accumulator acc _ = acc

toEither :: forall a. Maybe a -> Either String a
toEither value = case value of
  Just v -> Right v
  Nothing -> Left "Something went wrong in the interpreter (would be nice if this gave more detail)"
