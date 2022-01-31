module Json (Json(..), at) where


import Prelude (class Eq, class Show, ($))
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
  show _ = "TODO: Show Json"

at :: String -> Json -> Json
at key (JObject object) =
  fromMaybe JNull $ lookup key object
at _ _ = JNull
