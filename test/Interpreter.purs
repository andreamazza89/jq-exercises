module Test.Interpreter where

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Expression (Expression)
import Helpers.Expression (accessByKeyNames, identity)
import Interpreter (run) as Interpreter
import Json (Json)
import Json as Json
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Text.Parsing.Parser (runParser)

main :: Spec Unit
main = do
  describe "Interpreting" do
    describe "Identity" do
      it "identity" do
        test2 identity
          "4.2"
          "4.2"
    describe "Accessor" do
      it "gets the value in the object at the given keys" do
        test2 (accessByKeyNames [ "foo", "bar" ])
          """
            {
              "foo": { "bar": "ciao" }
            }
          """
          "\"ciao\""

test :: forall a. MonadThrow Error a => Expression -> Json -> Json -> a Unit
test expression input expectedOutput = Interpreter.run expression input `shouldEqual` (Right expectedOutput)

test2 :: forall a. MonadThrow Error a => Expression -> String -> String -> a Unit
test2 expression input expectedOutput = case [ runParser input Json.parser, runParser expectedOutput Json.parser ] of
  [ Right i, Right o ] -> test expression i o
  [ Left _, _ ] -> fail "failed to parse JSON in the input"
  [ _, Left _ ] -> fail "failed to parse JSON in the output"
  _ -> fail "failed to parse JSON"
