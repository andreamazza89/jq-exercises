module Test.Parser.Environment where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Effect.Exception (Error)
import Environment (Environment, JqFunction)
import Environment (empty, getFunction) as Env
import Expression (Expression(..))
import Helpers.Expression (identity) as Exp
import Helpers.Expression ((~))
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

testEnvWith :: forall a b. MonadThrow Error a => Show b => Eq b => String -> (Environment Expression -> b) -> b -> a Unit
testEnvWith expression modifyEnv expectedEnv = parsed `shouldEqual` (Right expectedEnv)
  where
  parsed = map (snd >>> modifyEnv) (parse expression)
