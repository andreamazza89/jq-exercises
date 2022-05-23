module Test.Parser.Environment where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (snd)
import Environment (addFunction, empty) as Environment
import Expression (Expression(..))
import Parser (parse)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  describe "Parsing Environment" do
    describe "Function definitions" do
      it "the environment can be empty" do
        let
          parsed = map snd (parse ".")
        parsed `shouldEqual` (Right Environment.empty)
      it "simple function definition at the beginning of the program" do
        let
          parsed = map snd (parse "def foo: .; .")
        parsed `shouldEqual` (Right (Environment.addFunction { name: "foo", body: Identity } Environment.empty))
