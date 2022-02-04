module Test.Parser where

import Helpers.Expression
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Expression (Expression)
import Parser (parse)
import Prelude (Unit, ($), discard)
import Test.Helpers.Json (num)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  describe "Parsing Expressions" do
    describe "Identity" do
      it "identity" do
        testParser "." identity
    describe "Accessors" do
      it "at key" do
        testParser ".foo" $ accessByKeyNames [ "foo" ]
      it "nested keys" do
        testParser ".foo.bar" $ accessByKeyNames [ "foo", "bar" ]
      it "at index" do
        testParser ".[2]" $ accessByIndex [ 2 ]
      it "iterate all items" do
        testParser ".[]" $ accessAllItems
      it "mixed access" do
        testParser ".foo[3].bar[]" $ accessor [ atKey "foo", atIndex 3, atKey "bar", allItems ]
    describe "Pipes" do
      it "pipe" do
        testParser ". | .foo | ." $ identity || accessByKeyNames [ "foo" ] || identity
    describe "ArrayConstructor" do
      it "array of identity" do
        testParser "[.]" $ constructArray identity
      it "array of literals" do
        testParser "[42]" $ constructArray (literal (num 42.0))

testParser :: forall a. MonadThrow Error a => String -> Expression -> a Unit
testParser source expected = parse source `shouldEqual` Right expected
