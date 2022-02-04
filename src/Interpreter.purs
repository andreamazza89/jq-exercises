module Interpreter (run) where

import Prelude (($))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Expression (Expression(..), Target(..))
import Json (at) as Json
import Json (Json)

-- 2 open questions that will affect the return type:
--   1 - Does this operation ever fail, or just default to Null? As in, given a valid expression and Json, can this ever
--       fail, or do we just return a JNull?
--  2 - I think perhaps we should return a Json Array ([Json]), as that's what jq does.
run :: Expression -> Json -> Either String Json
run (Accessor _ path) input = Right $ foldl accumulator input path

run _ input = Right input

accumulator :: Json -> Target -> Json
accumulator acc (Key k) = Json.at k acc

accumulator acc _ = acc
