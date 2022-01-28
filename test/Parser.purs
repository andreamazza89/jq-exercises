module Test.Main where

import Prelude (Unit, ($), discard)

import Data.Either (Either(..))
import Data.List.NonEmpty (singleton, cons') as NonEmpty
import Data.List (singleton) as List
import Effect (Effect)
import Effect.Aff (launchAff_)
import Filter (Filter(..))
import Parser (parse)
import Test.Spec (describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Parsing filters" do
          it "identity" do
            parse "." `shouldEqual` Right Identity
          it "select" do
            parse ".foo" `shouldEqual` Right (Select (NonEmpty.singleton "foo"))
          it "nested select" do
            parse ".foo.bar" `shouldEqual` Right (Select (NonEmpty.cons' "foo" $ List.singleton "bar"))
          it "pipe" do
            parse ". | ." `shouldEqual` Right (Pipe Identity Identity)

