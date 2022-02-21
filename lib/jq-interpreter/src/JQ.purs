module JQ where

import Prelude (bind, ($), (#), (<$>), (<*>), (==))
import Data.Either (hush)
import Data.Functor (map)
import Data.Maybe (Maybe, fromMaybe)
import Json (parse, serialise) as Json
import Interpreter (run) as Interpreter
import Parser (parse) as Parser

run :: String -> String -> Maybe (Array String)
run json jq = do
  input <- hush $ Json.parse json
  exp <- hush $ Parser.parse jq
  Interpreter.run exp [ input ]
    # map (map Json.serialise)
    # hush

jsonEquals :: String -> String -> Maybe Boolean
jsonEquals left right =
  (==) <$> Json.parse left <*> Json.parse right
    # hush
