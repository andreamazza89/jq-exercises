module Interpreter (run) where

import Data.Array (concat)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Expression (Expression(..), Target(..))
import Json (Json)
import Json (atIndex, atKey, buildArray, buildObject2, emptyArray, emptyObject, values) as Json
import Prelude (bind, pure, (#), ($), (<$>), (<*>), (<>), (>>=))
import Utils.CartesianProduct as Cartesian

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

run (ObjectConstructor []) _ = pure [ Json.emptyObject ]
run (ObjectConstructor keyValuePairs) input = do
  kvs <- one keyValuePairs input # map two # map Cartesian.combine-- map concat # map (map Array.singleton)
  maybe (Left "boom") pure (traverse Json.buildObject2 kvs)

  -- stuff <- map Json.buildObject $ run expression input
  -- maybe (Left "Fail to build json Object") (Array.singleton >>> pure) stuff

-- [(k, v), (k, v) ...]
  -- one
-- [([a], [2, 3]), ([b, c], [42]) ...]
  -- two
-- [ [(a,2), (a, 3)], [(b, 42), (c, 42)] ...]]
  -- three
-- [ [(a,2), (b, 42)], [(a, 3), (b, 42)], [(a,2), (c,42)]...]]

-- [[(k1,v1)], [(k1, v2)], ...]]

one :: Array (Tuple Expression Expression) -> Array Json -> Either String (Array (Tuple (Array Json) (Array Json)))
one arr input =
  sequence (map (\(Tuple key val) ->
    Tuple <$> run key input <*> run val input
  ) arr)

two :: Array (Tuple (Array Json) (Array Json)) -> Array (Array (Tuple Json Json))
two =
  map cartesian

-- three :: Array (Array (Tuple Json Json)) ->  Array (Array (Tuple Json Json))
-- three arr = 
--   where
--     go acc arr counter =

cartesian :: Tuple (Array Json) (Array Json) -> Array (Tuple Json Json)
cartesian (Tuple keys values) = do
  k <- keys
  v <- values
  pure $ Tuple k v
  

accessor :: Maybe (Array Json) -> Target -> Maybe (Array Json)
accessor acc (Key k) = acc >>= traverse (Json.atKey k)

accessor acc (AtIndex i) = acc >>= traverse (Json.atIndex i)

accessor acc Each = acc >>= traverse Json.values # map concat

toEither :: Maybe (Array Json) -> Either String (Array Json)
toEither output = case output of
  Just o -> Right o
  Nothing -> Left "Something went wrong in the interpreter (would be nice if this gave more detail)"
