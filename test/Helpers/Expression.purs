module Helpers.Expression where

import Prelude (($))
import Expression (Expression(..), Over(..), Target(..))
import Data.Array.NonEmpty (fromArray, singleton) as NE
import Data.Maybe (fromMaybe)
import Data.Functor (map)

-- Helpers to build Expressions
accessByKeyNames :: Array String -> Expression
accessByKeyNames keys = (Accessor Input (map Key (fromMaybe (NE.singleton "fix me") $ NE.fromArray keys)))

accessByIndex :: Array Int -> Expression
accessByIndex indexes = (Accessor Input (map AtIndex (fromMaybe (NE.singleton 42) $ NE.fromArray indexes)))

accessAllItems :: Expression
accessAllItems = Accessor Input (NE.singleton AllItems)

accessor targets = Accessor Input (fromMaybe (NE.singleton $ Key "fix me") $ NE.fromArray targets)

atKey = Key

atIndex = AtIndex

allItems = AllItems

identity :: Expression
identity = Identity

pipe = Pipe
infixl 6 pipe as ||
