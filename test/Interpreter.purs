module Test.Interpreter where

import Control.Monad.Error.Class (class MonadThrow)
import Expression (Expression)
import Data.Either (Either(..))
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Helpers.Expression (accessByKeyNames, identity)
import Interpreter (run) as Interpreter
import Json (Json(..))
import Prelude (Unit, discard, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  describe "Interpreting" do
    describe "Identity" do
      it "identity" do
        test identity (num 4.2) (num 4.2)
    describe "Accessor" do
      it "gets the value in the object at the given keys" do
        test (accessByKeyNames [ "foo", "bar" ]) input (str "ciao")
  where
  input =
    obj
      [ "foo"
          : obj
              [ "bar" : str "ciao"
              ]
      ]

test :: forall a. MonadThrow Error a => Expression -> Json -> Json -> a Unit
test expression input expectedOutput = Interpreter.run expression input `shouldEqual` (Right expectedOutput)

-- Helpers to build Json data - I am not convinced about these...It might be best to just parse from a Json string
num :: Number -> Json
num val = JNumber val

str :: String -> Json
str val = JString val

obj :: Array (Tuple String Json) -> Json
obj keys = JObject $ fromFoldable keys

tupe :: String -> Json -> Tuple String Json
tupe = Tuple

infixl 5 tupe as :
