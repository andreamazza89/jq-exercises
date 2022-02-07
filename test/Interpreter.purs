module Test.Interpreter where

import Helpers.Expression
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Expression (Expression)
import Interpreter (run) as Interpreter
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
        test identity
          "4.2"
          [ "4.2" ]
    describe "Accessor" do
      it "gets the value in the object at the given keys" do
        test (accessByKeyNames [ "foo", "bar" ])
          """
            {
              "foo": { "bar": "ciao" }
            }
          """
          [ "\"ciao\"" ]
      it "gets the value in the array at the given index" do
        test (accessByIndex [ 1 ])
          """
            ["ciao", "miao"]
          """
          [ "\"miao\"" ]
      it "gets the value at the given mixed path" do
        test (accessor [ atKey "ciao", atIndex 1, atKey "miao" ])
          """
            {
              "ciao": [
                42,
                { "miao": true }
              ],
              "somethingElse": 33
            }
          """
          [ "true" ]
      it "iterates over the items of an array" do
        test (accessAllItems)
          """
            ["ciao", "miao"]
          """
          [ "\"ciao\"", "\"miao\"" ]
      it "nested array iteration" do
        test (accessAllItems || accessByKeyNames [ "nest" ] || accessAllItems)
          """
            [
              { "nest" : [33, "wat"] },
              { "nest" : [true, "gotta love mixing types"] }
            ]
          """
          [ "33", "\"wat\"", "true", "\"gotta love mixing types\"" ]
    describe "Pipe" do
      it "simple pipe" do
        test (accessByIndex [ 0 ] || identity)
          """
            ["ciao", "miao"]
          """
          [ "\"ciao\"" ]

test :: forall a. MonadThrow Error a => Expression -> String -> Array String -> a Unit
test expression input expectedOutput = case Tuple (parseJson input) (traverse parseJson expectedOutput) of
  Tuple (Right i) (Right o) -> Interpreter.run expression [ i ] `shouldEqual` (Right o)
  _ -> fail "failed to parse JSON"
  where
  parseJson s = runParser s Json.parser
