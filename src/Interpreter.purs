module Interpreter (run) where

import Prelude (($))
import Data.Either (Either(..))
import Data.Array (foldl)
import Expression (Expression(..), Target(..))
import Json (Json, at)

run :: Expression -> Json -> Either String Json
run (Accessor _ path) input = Right $ foldl accumulator input path
run _ input = Right input

accumulator ::  Json -> Target -> Json
accumulator acc (Key k) = at k acc
accumulator acc _  = acc
