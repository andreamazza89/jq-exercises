module Expression where

import Prelude (class Eq, class Show, show, (<>))

import Data.List.Types (NonEmptyList)

data Expression =
    Identity
  | Select (NonEmptyList String)
  | Pipe Expression Expression

derive instance equalExpression :: Eq Expression

instance Show Expression where
  show Identity = "Identity"
  show (Select selector) = "Select " <> show selector
  show (Pipe l r) = "Pipe || " <> show l <> " , " <> show r <> " ||"


