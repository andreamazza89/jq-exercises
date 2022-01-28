module Test.Interpreter where

import Prelude (Unit)
import Expression (Expression(..))
import Interpreter (run) as Interpreter
import Test.Spec (Spec, describe, it)
import Json
import Data.Either (Either(..))
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  describe "Interpreting" do
    describe "Identity" do
      it "identity" do
        Interpreter.run Identity (JNumber 4.2) `shouldEqual` (Right (JNumber 4.2))
