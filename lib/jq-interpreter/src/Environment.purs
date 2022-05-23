module Environment
  ( Environment(..) -- TODO - make me opaque pleaseeeee?
  , addFunction
  , empty
  ) where

import Expression (Expression)
import Prelude (class Show, class Eq)

data Environment
  = Environment

type FunctionOptions = { name :: String, body :: Expression}

derive instance environmentEq :: Eq Environment

instance Show Environment where
  show _ = "TODO - better show instance"

empty :: Environment
empty = Environment

addFunction :: FunctionOptions -> Environment -> Environment
addFunction options environment = empty