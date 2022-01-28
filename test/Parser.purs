module Test.Main where

import Prelude (Unit, ($), discard)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error)
import Expression (Expression(..),  Over(..), Target(..))
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
          describe "Identity" do
            it "identity" do
              testParser "." Identity
          describe "Accessors" do
            it "at key" do
              testParser ".foo" $ Accessor Input [Key "foo"]
            it "nested keys" do
              testParser ".foo.bar" $ Accessor Input [Key "foo", Key "bar"]
          describe "Pipes" do
            it "pipe" do
              testParser ". | .foo | ." $ Pipe (Pipe Identity (Accessor Input [Key "foo"])) Identity

testParser :: forall a. MonadThrow Error a => String -> Expression -> a Unit
testParser source expected = parse source `shouldEqual` Right expected
