module Filter where

import Prelude (class Eq, class Show, show, (<>))

import Data.List.Types (NonEmptyList)

data Filter =
    Identity
  | Select (NonEmptyList String)

derive instance equalFilter :: Eq Filter

instance Show Filter where
  show Identity = "Identity"
  show (Select selector) = "Select " <> show selector

