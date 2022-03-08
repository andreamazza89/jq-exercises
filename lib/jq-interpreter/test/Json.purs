module Test.Json where

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Json (index, everyItem, key)
import Json as Json
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it, itOnly)
import Test.Spec.Assertions (fail, shouldEqual)

main :: Spec Unit
main = do
  describe "Json" do
    describe "Reads" do
      it "an empty path refers to the whole json structure" do
        testRead [] """ 42 """ [ """ 42 """ ]
      it "reads at the specified key" do
        testRead [ key "pizza" ]
          """
            {
                "pizza": "Margherita"
            }
          """
          [ """ "Margherita" """ ]
      it "reads at the specified index" do
        testRead [ index 1 ]
          """
            [ "Salsiccia",
              "Patate"
            ]
          """
          [ """ "Patate" """ ]
      it "reads all object values (sorted by key)" do
        testRead [ everyItem ]
          """
            {
              "zz": "second value",
              "aa": "first value"
            }
          """
          [ """ "first value" """, """ "second value" """ ]
      it "reads all array values" do
        testRead [ everyItem ]
          """
            [
              "Croque",
              "Monsieur"
            ]
          """
          [ """ "Croque" """, """ "Monsieur" """ ]
      it "mixed access" do
        testRead [ key "pizza", everyItem, index 1 ]
          """
            {
                "pizza": [ ["Senape", "Patate"], ["OtherStuff", "Funghi"] ]
            }
          """
          [ """ "Patate" """, """ "Funghi" """ ]
    describe "Updates" do
      it "an empty path refers to the whole json structure" do
        testUpdate []
          """
            {
                "iLove": "bread"
            }
          """
          [ """42""" ]
          """42"""
      it "replaces value in an object" do
        testUpdate [ key "pizza", key "name" ]
          """
              {
                  "pizza": { "name": "Bianca" }
              }
          """
          [ """ "Napoletana" """ ]
          """
              {
                  "pizza": { "name": "Napoletana" }
              }
          """
      it "replaces value in an array" do
        testUpdate [ index 1 ]
          """
              [
                  "Panino",
                  "Spanner"
              ]
              
          """
          [ """ "Lasagna" """ ]
          """
              [
                  "Panino",
                  "Lasagna"
              ]
          """
      it "updates all object values (sorted by key)" do
        testUpdate [ everyItem ]
          """
            {
              "zz": "bread",
              "aa": "pasta"
            }
          """
          [ """ "first" """, """ "second" """ ]
          """
            {
              "zz": "second",
              "aa": "first"
            }
          """
      it "updates all array values" do
        testUpdate [ everyItem ]
          """
            [1, 2, 3]
          """
          [ """42""", """43""", """44""" ]
          """
            [42, 43, 44]
          """
      it "replaces values using a mixed path" do
        testUpdate [ key "pizza", everyItem, index 0 ]
          """
            {
                "pizza": [ ["Senape", "Patate"], ["OtherStuff", "Funghi"] ]
            }
          """
          [ """ "Salsiccia" """, """ "Quattro Stagioni" """ ]
          """
            {
                "pizza": [ ["Salsiccia", "Patate"], ["Quattro Stagioni", "Funghi"] ]
            }
          """

testUpdate :: forall a. MonadThrow Error a => Json.Path -> String -> Array String -> String -> a Unit
testUpdate path json newValues expected = case { j: Json.parse json, new: traverse Json.parse newValues, exp: Json.parse expected } of
  { j: (Right json'), new: (Right newValues'), exp: (Right expected') } -> Json.update path newValues' json' `shouldEqual` Right expected'
  _ -> fail "Json test definition failed to parse"

testRead :: forall a. MonadThrow Error a => Json.Path -> String -> Array String -> a Unit
testRead path json expected = case (Tuple (Json.parse json) (traverse Json.parse expected)) of
  Tuple (Right json') (Right expected') -> Json.atPath path json' `shouldEqual` Right expected'
  _ -> fail "Json test definition failed to parse"
