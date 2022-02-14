module Interpreter (run) where

import Data.Array (concat)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Expression (Expression(..), Target(..))
import Json (Json)
import Json (atIndex, atKey, buildArray, buildObject, emptyArray, emptyObject, values) as Json
import Prelude (bind, pure, (#), ($), (<>), (>>=), (>>>))

run :: Expression -> Array Json -> Either String (Array Json)
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

run (ObjectConstructor Nothing) _ = pure [ Json.emptyObject ]

run (ObjectConstructor (Just expression)) input = do
  stuff <- map Json.buildObject $ run expression input
  maybe (Left "Fail to build json Object") (Array.singleton >>> pure) stuff


accessor :: Maybe (Array Json) -> Target -> Maybe (Array Json)
accessor acc (Key k) = acc >>= traverse (Json.atKey k)

accessor acc (AtIndex i) = acc >>= traverse (Json.atIndex i)

accessor acc Each = acc >>= traverse Json.values # map concat

toEither :: Maybe (Array Json) -> Either String (Array Json)
toEither output = case output of
  Just o -> Right o
  Nothing -> Left "Something went wrong in the interpreter (would be nice if this gave more detail)"
