module Expression (Expression(..), Over(..), Path, Target(..)) where

import Prelude (class Eq, class Show, show, (<>))
import Data.List.Types (NonEmptyList)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe
import Data.String (joinWith)
import Data.Functor (map)

data Expression
  = Identity
  | Accessor Over Path
  | Pipe Expression Expression

-- This ought to be a NonEmptyArray, but I'm making a compromise to be able to use array literals
type Path
  = Array Target

data Over
  = AnExpression Expression
  | Input

data Target
  = Key String
  | AtIndex Int
  | WholeArray

derive instance equalExpression :: Eq Expression

derive instance equalOver :: Eq Over

derive instance equalTarget :: Eq Target

instance Show Expression where
  show Identity = "Identity"
  show (Pipe l r) = show l <> " || " <> show r
  show (Accessor Input path) = "." <> joinPath path
  show (Accessor (AnExpression expression) path) = show expression <> "." <> joinPath path

joinPath :: Path -> String
joinPath path = joinWith "." (map show path)

instance Show Target where
  show (Key k) = k
  show (AtIndex index) = "[" <> show index <> "]"
  show WholeArray = "[]"
