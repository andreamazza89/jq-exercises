module Test.Json where

import Helpers.Expression

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Expression (Expression, accessByKeyName)
import Interpreter (run) as Interpreter
import Json (index, key)
import Json as Json
import Prelude (Unit, discard, pure, unit, (+))
import Test.Helpers.Json (num, str)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Text.Parsing.Parser (runParser)

main :: Spec Unit
main = do
  describe "Json" do
    describe "Updates" do
      it "an empty path refers to the whole json structure" do
        test []
          """
            {
                "iLove": "bread"
            }
          """
          """42"""
          """42"""
      it "replaces value in an object" do
        test [ key "pizza", key "name" ]
          """
              {
                  "pizza": { "name": "Bianca" }
              }
          """
          """ "Napoletana" """
          """
              {
                  "pizza": { "name": "Napoletana" }
              }
          """
      it "replaces value in an array" do
        test [ index 1 ]
          """
              [
                  "Panino",
                  "Spanner"
              ]
              
          """
          """ "Lasagna" """
          """
              [
                  "Panino",
                  "Lasagna"
              ]
          """
      it "replaces value using a mixed path" do
        test [ index 1, key "food" ]
          """
              [
                  {"food": "milk"},
                  {"food": "bread"}
              ]
              
          """
          """ {"type": "bread", "weight": 42} """
          """
              [
                  {"food": "milk"},
                  {"food": {"type": "bread", "weight": 42}}
              ]
          """

test :: forall a. MonadThrow Error a => Json.Path -> String -> String -> String -> a Unit
test path json newValue expected = case [ Json.parse json, Json.parse newValue, Json.parse expected ] of
  [ Right json', Right newValue', Right expected' ] -> Json.update path newValue' json' `shouldEqual` Right expected'
  _ -> fail "Json test definition failed to parse"
