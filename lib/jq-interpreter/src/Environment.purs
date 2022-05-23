module Environment
  ( Environment(..) -- TODO - make me opaque pleaseeeee?
  , Arity
  , FunctionOptions
  , addFunction
  , getFunction
  , empty
  ) where

import Data.Array (length)
import Data.Map (Map)
import Data.Map (empty, insert, lookup) as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Expression (Expression)
import Prelude (class Eq, class Show, map, (#))

data Environment
  = Environment { functions :: Map (Tuple String Arity) Expression }

type Arity
  = Int

-- The 'ingredients' required to add a function to the environment
type FunctionOptions
  = { name :: String
    , arity :: Arity
    , body :: Expression
    }

-- The output given when looking up a function in the Environment
-- The `environment` field is a local environment based on the given arguments; for example, say we have function
--    foo(bar): bar + 1;
-- if we lookup `foo` with
--    getFunction "foo" [identity]
-- then `body` will be something like `apply(bar) + 1`
-- and `environment` will contain the function `bar`, which is bound to the identity function
type FunctionOutput
  = { body :: Expression
    , environment :: Environment
    }

derive instance environmentEq :: Eq Environment

instance Show Environment where
  show _ = "TODO - better show instance"

empty :: Environment
empty = Environment { functions: Map.empty }

addFunction :: FunctionOptions -> Environment -> Environment
addFunction { name, arity, body } (Environment env) =
  Environment
    ( env
        { functions = Map.insert (Tuple name arity) (body) env.functions
        }
    )

getFunction :: String -> Array Expression -> Environment -> Maybe FunctionOutput
getFunction name arguments (Environment { functions }) =
  Map.lookup (Tuple name (length arguments)) functions
    # map (\exp -> { body: exp, environment: empty })
