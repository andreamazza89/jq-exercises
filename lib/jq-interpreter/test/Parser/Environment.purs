module Test.Parser.Environment where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Environment (empty, getFunction) as Env
import Expression (Expression(..))
import Parser (parse)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
-- TODO - CLEANUP THESE ASSERTIONS
  describe "Parsing Environment" do
    describe "Function definitions" do
      it "the environment can be empty" do
        let
          parsed = map snd (parse ".")
        parsed `shouldEqual` (Right Env.empty)
      it "simple function definition at the beginning of the program" do
        let
          parsed = map (snd >>> (Env.getFunction "foo" [])) (parse "def foo: .; .")
        parsed `shouldEqual` (Right (Just { body: Identity, environment: Env.empty }))
