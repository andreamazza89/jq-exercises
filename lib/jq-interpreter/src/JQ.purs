module JQ where

import Data.Either (Either, hush)
import Data.Functor (map)
import Data.Maybe (Maybe)
import Data.Tuple (fst)
import Interpreter (run) as Interpreter
import Json (parse, serialise) as Json
import Parser (parse) as Parser
import Prelude (bind, map, (#), (<$>), (<*>), (==))

run :: String -> String -> Either String (Array String)
run json jq = do
  input <- Json.parse json
  exp <-
    Parser.parse jq
      # map fst
  Interpreter.run exp [ input ]
    # map (map Json.serialise)

jsonEquals :: String -> String -> Maybe Boolean
jsonEquals left right =
  (==) <$> Json.parse left <*> Json.parse right
    # hush
