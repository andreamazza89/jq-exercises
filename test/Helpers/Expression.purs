module Helpers.Expression where

import Expression (Expression(..), Over(..), Target(..))
import Data.Functor (map)

-- Helpers to build Expressions
accessByKeyNames :: Array String -> Expression
accessByKeyNames keys = (Accessor Input (map Key keys))

identity :: Expression
identity = Identity

pipe = Pipe
infixl 6 pipe as ||
