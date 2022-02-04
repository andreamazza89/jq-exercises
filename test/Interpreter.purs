module Test.Interpreter where

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Expression (Expression)
import Helpers.Expression (accessByKeyNames, identity)
import Interpreter (run) as Interpreter
import Json (Json)
import Prelude (Unit, discard)
import Test.Helpers.Json
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
          : obj [ "bar" : str "ciao" ]
      ]

test :: forall a. MonadThrow Error a => Expression -> Json -> Json -> a Unit
test expression input expectedOutput = Interpreter.run expression input `shouldEqual` (Right expectedOutput)
