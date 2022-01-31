module Json (Json(..), at) where

import Prelude (class Eq, class Show, ($), show)
import Data.Map (Map, lookup)
import Data.Maybe (fromMaybe)

data Json
  = JNull
  | JString String
  | JNumber Number
  | JBoolean Boolean
  | JArray (Array Json)
  | JObject (Map String Json)

derive instance equalJson :: Eq Json

instance Show Json where
  show JNull = "null"
  show (JString s) = s
  show (JNumber n) = show n
  show (JBoolean b) = show b
  show (JArray a) = show a
  show (JObject o) = show o

at :: String -> Json -> Json
at key (JObject object) = fromMaybe JNull $ lookup key object

at _ _ = JNull
