module Environment
  ( Environment
  , Arity
  , FunctionOptions
  , FunctionArgs
  , FunctionKey
  , FunctionName
  , JqFunction
  , fromFunction
  , getFunction
  , empty
  ) where

import Data.Array (length)
import Data.Map (Map)
import Data.Map (empty, insert, lookup) as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, map, (#))

data Environment expression
  = Environment { functions :: Map FunctionKey expression }

type FunctionKey
  = Tuple FunctionName Arity

type FunctionName
  = String

type Arity
  = Int

type FunctionArgs exp = Array exp

-- The 'ingredients' required to add a function to the environment
type FunctionOptions expression
  = { name :: String
    , arity :: Arity
    , body :: expression
    }

-- The output given when looking up a function in the Environment
-- The `environment` field is a local environment based on the given arguments; for example, say we have function
--    foo(bar): bar + 1;
-- if we lookup `foo` with
--    getFunction "foo" [identity]
-- then `body` will be something like `apply(bar) + 1`
-- and `environment` will contain the function `bar`, which is bound to the identity function
type JqFunction expression
  = { body :: expression
    , environment :: Environment expression
    }

derive instance environmentEq :: (Eq exp) => Eq (Environment exp)

instance Show (Environment env) where
  show _ = "TODO - better show instance"

empty :: forall exp. Environment exp
empty = Environment { functions: Map.empty }

fromFunction :: forall exp. FunctionOptions exp -> Environment exp
fromFunction { name, arity, body }  =
  Environment { functions: Map.insert (Tuple name arity) (body) Map.empty }


getFunction :: forall exp. String -> FunctionArgs exp -> Environment exp -> Maybe (JqFunction exp)
getFunction name arguments (Environment { functions }) =
  Map.lookup (Tuple name (length arguments)) functions
    # map (\exp -> { body: exp, environment: empty })
