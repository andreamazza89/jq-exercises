module Test.Interpreter where

import Control.Monad.Error.Class (class MonadThrow)
import Expression (Expression(..), Over(..), Target(..))
import Data.Either (Either(..))
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Interpreter (run) as Interpreter
import Json
import Prelude (Unit, discard, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  describe "Interpreting" do
    describe "Identity" do
      it "identity" do
        test Identity (JNumber 4.2) (Right (JNumber 4.2))
    describe "Accessor" do
      it "gets the value in the object at the given keys" do
        test
          (Accessor Input [Key "foo", Key "bar"])
          (JObject $ fromFoldable [Tuple "foo" (JObject $ fromFoldable [Tuple "bar" (JString "ciao")])]) (Right (JString "ciao"))

test :: forall a. MonadThrow Error a => Expression -> Json -> Either String Json -> a Unit
test expression input expectedOutput =
  Interpreter.run expression input `shouldEqual` expectedOutput
