module Test.Parser.Environment where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Writer (pass)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Effect.Exception (Error)
import Environment (Environment, JqFunction)
import Environment (empty, getFunction) as Env
import Expression (Expression(..))
import Helpers.Expression ((~))
import Helpers.Expression (identity) as Exp
import Parser (parse)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

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
          (Just { body: Exp.identity, environment: Env.empty })
      it "defining multiple functions" do
        testEnvWith
          "def foo: .; def bar: ., .; ."
          (\e -> Tuple <$> (Env.getFunction "foo" [] e) <*> (Env.getFunction "bar" [] e))
          ( Just
              ( Tuple
                  { body: Exp.identity, environment: Env.empty }
                  { body: Exp.identity ~ Exp.identity, environment: Env.empty }
              )
          )
      it "a malformed definition should fail to parse" do
        testEnvFailure
          "def : .;"
          "malformed environment"

testEnvWith :: forall a b. MonadThrow Error a => Show b => Eq b => String -> (Environment Expression -> b) -> b -> a Unit
testEnvWith expression modifyEnv expectedEnv = parsed `shouldEqual` (Right expectedEnv)
  where
  parsed = map (snd >>> modifyEnv) (parse expression)

testEnvFailure :: forall a. MonadThrow Error a => String -> String -> a Unit
testEnvFailure expression expectedErrorMsg = case parse expression of
  Right _ -> fail "Should have failed to parse"
  Left error -> expectedErrorMsg `shouldEqual` error
