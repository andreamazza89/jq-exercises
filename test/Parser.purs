module Test.Main where

import Prelude (Unit, ($), discard)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.List.NonEmpty (singleton, cons') as NonEmpty
import Data.List (singleton) as List
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error)
import Expression (Expression(..))
import Parser (parse)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Parsing Expressions" do
          it "identity" do
            testParser "." Identity
          it "select" do
            testParser " .foo " $ Select (NonEmpty.singleton "foo")
          it "nested select" do
            testParser ".foo.bar" $ Select (NonEmpty.cons' "foo" $ List.singleton "bar")
          it "pipe" do
            testParser ". | .foo | ." $ Pipe (Pipe Identity (Select (NonEmpty.singleton "foo"))) Identity

testParser :: forall a. MonadThrow Error a => String -> Expression -> a Unit
testParser source expected = parse source `shouldEqual` Right expected
