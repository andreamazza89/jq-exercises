module Interpreter (run) where

import Data.Either (Either(..))
import Expression (Expression)
import Json (Json)

run :: Expression -> Json -> Either String Json
run _ input = Right input
