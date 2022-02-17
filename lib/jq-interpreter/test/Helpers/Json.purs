module Test.Helpers.Json
  ( (:)
  , num
  , obj
  , str
  , tupe
  ) where

import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import Json (Json(..))
import Prelude (($))

-- Helpers to build Json data - I am not convinced about these...It might be best to just parse from a Json string
num :: Number -> Json
num val = JNumber val

str :: String -> Json
str val = JString val

obj :: Array (Tuple String Json) -> Json
obj keys = JObject $ fromFoldable keys

tupe :: String -> Json -> Tuple String Json
tupe = Tuple

infixl 5 tupe as :
