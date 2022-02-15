module Test.CartesianProduct where

import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Utils.CartesianProduct as Cartesian

main :: Spec Unit
main = do
  describe "Cartesian Product" do
    it "empty result on empty input" do
      Cartesian.combine empty_ `shouldEqual` empty_
    it "single collection" do
      Cartesian.combine [ letters ] `shouldEqual` [ [ "a" ], [ "b" ] ]
    it "combines multiple collections" do
      Cartesian.combine [ letters, numbers, symbols ]
        `shouldEqual`
          [ [ "a", "1", "*" ]
          , [ "a", "1", "~" ]
          , [ "a", "2", "*" ]
          , [ "a", "2", "~" ]
          , [ "b", "1", "*" ]
          , [ "b", "1", "~" ]
          , [ "b", "2", "*" ]
          , [ "b", "2", "~" ]
          ]

letters :: Array String
letters = [ "a", "b" ]

numbers :: Array String
numbers = [ "1", "2" ]

symbols :: Array String
symbols = [ "*", "~" ]

empty_ :: Array (Array Int)
empty_ = [ [] ]
