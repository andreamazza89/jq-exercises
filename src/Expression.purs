module Expression (Expression(..), Over(..), Path, Target(..)) where

import Prelude (class Eq, class Show, show, (<>))
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.String (joinWith)
import Data.Functor (map)

data Expression
  = Identity
  | Accessor Over Path
  | Pipe Expression Expression

type Path
  = NonEmptyArray Target

data Over
  = AnExpression Expression
  | Input

data Target
  = Key String
  | AtIndex Int
  | Each

derive instance equalExpression :: Eq Expression

derive instance equalOver :: Eq Over

derive instance equalTarget :: Eq Target

instance Show Expression where
  show Identity = "Identity"
  show (Pipe l r) = show l <> " || " <> show r
  show (Accessor Input path) = "." <> joinPath path
  show (Accessor (AnExpression expression) path) = show expression <> "." <> joinPath path

joinPath :: Path -> String
joinPath path = joinWith "." (map show (toArray path))

instance Show Target where
  show (Key k) = "\'" <> k <> "\'"
  show (AtIndex index) = "[" <> show index <> "]"
  show Each = "[]"
