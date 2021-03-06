module Expression
  ( Expression(..)
  , KeyValuePair(..)
  , Over(..)
  , Path
  , Target(..)
  , accessByKeyName
  , toJsonPath
  , toJsonPaths
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, singleton, toArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple)
import Environment (FunctionName)
import Json (Json)
import Json (Path, Target, key, everyItem, index) as Json

data Expression
  = Identity
  | Accessor Over Path
  | Literal Json
  | ArrayConstructor (Maybe Expression)
  | ObjectConstructor (Array KeyValuePair)
  | Pipe Expression Expression
  | Comma Expression Expression
  | Update Expression Expression
  | Apply FunctionName

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

-- Turning an expression into a Json path
toJsonPaths :: Expression -> Either String (Array Json.Path)
toJsonPaths Identity = Right []

toJsonPaths (Accessor Input path) =
  toJsonPath path
    # Array.singleton
    # Right

toJsonPaths (Comma l r) = do
  lPaths <- toJsonPaths l
  rPaths <- toJsonPaths r
  pure (lPaths <> rPaths)

toJsonPaths exp = Left ("expression cannot be used to access a json value: " <> show exp)

toJsonPath :: Path -> Json.Path
toJsonPath path =
  map toJsonTarget path
    # Array.fromFoldable

toJsonTarget :: Target -> Json.Target
toJsonTarget (Key k) = Json.key k

toJsonTarget (AtIndex i) = Json.index i

toJsonTarget Each = Json.everyItem

-- Show
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
  show (Update l r) = show l <> " |= " <> show r
  show (Apply name) = name <> "(.)"

joinPath :: Path -> String
joinPath path = joinWith "." (map show (toArray path))

instance Show Target where
  show (Key k) = "\'" <> k <> "\'"
  show (AtIndex index) = "[" <> show index <> "]"
  show Each = "[]"

-- Constructors (this is a work in progress and should be reconciled with Helpers.Expression)
accessByKeyName :: String -> Expression
accessByKeyName = Key >>> singleton >>> Accessor Input
