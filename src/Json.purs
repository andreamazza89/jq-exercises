module Json (Json(..)) where


import Prelude (class Eq, class Show)
import Data.Map (Map)

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
