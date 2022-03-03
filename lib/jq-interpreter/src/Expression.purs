module Expression
  ( Expression(..)
  , Over(..)
  , Path
  , Target(..)
  , KeyValuePair(..)
  , accessByKeyName
  ) where

import Data.Array.NonEmpty (NonEmptyArray, singleton, toArray)
import Data.Functor (map)
import Data.Maybe (Maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple)
import Json (Json)
import Prelude (class Eq, class Show, show, (<>), (>>>), ($))

data Expression
  = Identity
  | Accessor Over Path
  | Literal Json
  | ArrayConstructor (Maybe Expression)
  | ObjectConstructor (Array KeyValuePair)
  | Pipe Expression Expression
  | Comma Expression Expression

type KeyValuePair
  = Tuple Expression Expression

type Path
  = NonEmptyArray Target

data Over
  = AnExpression Expression
  | Input

data Target
  = Key String
  | AtIndex Int
  | Each

type KeyValue
  = Tuple

derive instance equalExpression :: Eq Expression

derive instance equalOver :: Eq Over

derive instance equalTarget :: Eq Target

instance Show Expression where
  show Identity = "Identity"
  show (Pipe l r) = show l <> " || " <> show r
  show (ArrayConstructor items) = show items
  show (ObjectConstructor keyValues) = show keyValues
  show (Literal json) = show json
  show (Accessor Input path) = "." <> joinPath path
  show (Accessor (AnExpression expression) path) = show expression <> "." <> joinPath path
  show (Comma l r) = show l <> " , " <> show r

joinPath :: Path -> String
joinPath path = joinWith "." (map show (toArray path))

instance Show Target where
  show (Key k) = "\'" <> k <> "\'"
  show (AtIndex index) = "[" <> show index <> "]"
  show Each = "[]"

-- Constructors (this is a work in progress and should be reconciled with Helpers.Expression)
accessByKeyName :: String -> Expression
accessByKeyName = Key >>> singleton >>> Accessor Input
