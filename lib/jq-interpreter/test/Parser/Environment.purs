module Test.Parser.Environment where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Effect.Exception (Error)
import Environment (Environment)
import Environment (empty, getFunction) as Env
import Expression (Expression(..))
import Parser (parse)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  describe "Parsing Environment" do
    describe "Function definitions" do
      it "the environment can be empty" do
        testEnvWith
          "."
          identity
          Env.empty
      it "setting and retrieving a function from the environment" do
        testEnvWith
          "def foo: .; ."
          (Env.getFunction "foo" [])
          (Just { body: Identity, environment: Env.empty })

testEnvWith :: forall a b. MonadThrow Error a => Show b => Eq b => String -> (Environment Expression -> b) -> b -> a Unit
testEnvWith expression modifyEnv expectedEnv = parsed `shouldEqual` (Right expectedEnv)
  where
  parsed = map (snd >>> modifyEnv) (parse expression)
