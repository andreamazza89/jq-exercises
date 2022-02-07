module Interpreter (run) where

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Expression (Expression(..), Target(..))
import Json (Json)
import Json (atKey, atIndex) as Json
import Prelude ((#), (>>=))

-- 1 open question that will affect the return type:
--  - I think perhaps we should return a Json Array ([Json]), as that's what jq does.
run :: Expression -> Json -> Either String Json
run (Identity)  input = Right input
run (Accessor _ path) input =
  foldl accumulator (Just input) path
    # toEither
run (Pipe l r) input = run l input >>= run r
run _ _ = Left "TODO - interpreter support"

accumulator :: Maybe Json -> Target -> Maybe Json
accumulator acc (Key k) = acc >>= Json.atKey k

accumulator acc (AtIndex i) = acc >>= Json.atIndex i

accumulator acc _ = acc

toEither :: forall a. Maybe a -> Either String a
toEither value = case value of
  Just v -> Right v
  Nothing -> Left "Something went wrong in the interpreter (would be nice if this gave more detail)"
