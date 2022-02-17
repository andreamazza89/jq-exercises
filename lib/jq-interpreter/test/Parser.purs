module Test.Parser where

import Helpers.Expression
import Test.Helpers.Json

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Expression (Expression)
import Parser (parse)
import Prelude (Unit, ($), discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  describe "Parsing Expressions" do
    describe "Literal" do
      it "number" do
        testParser "42" $ literal (num 42.0)
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
    describe "ArrayConstructor" do
      it "empty array" do
        testParser "[]" $ constructEmptyArray
      it "identity" do
        testParser "[ . ]" $ constructArray identity
      it "literals" do
        testParser "[ 42.42, . ]" $ constructArray (literal (num 42.42) ~ identity)
      it "with pipe in it" do
        testParser "[ 42 | . ]" $ constructArray (literal (num 42.0) || identity)
      it "nested" do
        testParser "[ [ 42 ] ]" $ constructArray (constructArray (literal (num 42.0)))
      it "array and then pipe" do
        testParser "[ 42 ] | ." $ constructArray (literal (num 42.0)) || identity
    describe "ObjectConstructor" do
      it "empty object" do
        testParser "{}" $ constructEmptyObject
      it "simple object" do
        testParser
          """
            { "foo": 99 }
          """
          (constructObject [(Tuple (literal (str "foo")) (literal (num 99.0)))])
      it "object with expressions and literals" do
        testParser
          """
            { "miao": .bar,
              .: "ciao"
            }
          """
          (constructObject
            [
              Tuple (literal (str "miao")) (accessByKeyNames ["bar"])
            , Tuple (identity)             (literal (str "ciao"))
            ]
          )
    describe "Pipes" do
      it "pipe" do
        testParser ". | .foo | ." $ identity || accessByKeyNames [ "foo" ] || identity
    describe "Comma" do
      it "simple commas" do
        testParser ". , 42 , ." $ identity ~ (literal (num 42.0)) ~ identity
    describe "Operator Precedence" do
      it "comma has higher precedence than pipe" do
        testParser ". | 42 , ." $ identity || ((literal (num 42.0)) ~ identity)
      it "parentheses override precedence" do
        testParser "(. | 42) , ." $ (identity || (literal (num 42.0))) ~ identity

testParser :: forall a. MonadThrow Error a => String -> Expression -> a Unit
testParser source expected = parse source `shouldEqual` Right expected
