module Test.Interpreter where

import Helpers.Expression

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Environment (Environment)
import Environment (Environment, FunctionOptions)
import Environment (empty, fromFunction) as Env
import Expression (Expression)
import Interpreter (run) as Interpreter
import Json as Json
import Prelude (Unit, discard, pure, unit)
import Test.Helpers.Json (num, str)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Text.Parsing.Parser (runParser)

main :: Spec Unit
main = do
  describe "Interpreting" do
    describe "Identity" do
      it "identity" do
        test (literal (num 42.42))
          jsonInputIgnored
          [ "42.42" ]
    describe "Literal" do
      it "literal" do
        test identity
          "4.2"
          [ "4.2" ]
    describe "Accessor" do
      it "gets the value in the object at the given keys" do
        test (accessByKeyNames [ "foo", "bar" ])
          """
            {
              "foo": { "bar": "ciao" }
            }
          """
          [ "\"ciao\"" ]
      it "gets the value in the array at the given index" do
        test (accessByIndex [ 1 ])
          """
            ["ciao", "miao"]
          """
          [ "\"miao\"" ]
      it "null when no value found (array)" do
        test (accessByIndex [ 1 ])
          """
            ["ciao"]
          """
          [ "null" ]
      it "null when no value found (object)" do
        test (accessByKeyNames [ "foo" ])
          """
            {}
          """
          [ "null" ]
      it "gets the value at the given mixed path" do
        test (accessor [ atKey "ciao", atIndex 1, atKey "miao" ])
          """
            {
              "ciao": [
                42,
                { "miao": true }
              ],
              "somethingElse": 33
            }
          """
          [ "true" ]
      it "iterates over the items of an array" do
        test (accessAllItems)
          """
            ["ciao", "miao"]
          """
          [ "\"ciao\"", "\"miao\"" ]
      it "nested array iteration" do
        test (accessor [ allItems, atKey "nest", allItems ])
          """
            [
              { "nest" : [33, "wat"] },
              { "nest" : [true, "gotta love mixing types"] }
            ]
          """
          [ "33", "\"wat\"", "true", "\"gotta love mixing types\"" ]
    describe "Array constructor" do
      it "builds an empty array" do
        test (constructEmptyArray)
          jsonInputIgnored
          [ "[]" ]
      it "builds an an array from the input" do
        test (constructArray (accessByKeyNames [ "zero" ] ~ accessByKeyNames [ "one" ]))
          """
            {
              "zero": 0,
              "one": 1
            }
          """
          [ "[0, 1]" ]
      it "flattens nested arrays" do
        test (constructArray (accessor [ atKey "zero", allItems ] ~ accessByKeyNames [ "three" ]))
          """
            {
              "zero": [0,1,2],
              "three": 3
            }
          """
          [ "[0,1,2,3]" ]
    describe "Object constructor" do
      it "builds an empty object" do
        test (constructEmptyObject)
          jsonInputIgnored
          [ "{}" ]
      it "builds a simple object with literals" do
        test (constructObject [ Tuple (literal (str "ciao")) (literal (num 42.0)) ])
          jsonInputIgnored
          [ """
              { "ciao": 42 }
            """
          ]
      it "fails if an object key is not a string" do
        testFailure (constructObject [ Tuple (literal (num 42.0)) (literal (num 42.0)) ])
          jsonInputIgnored
      it "builds multiple objects when keys and/or values yield multiple outputs" do
        test
          ( constructObject
              [ Tuple (literal (str "ciao")) (accessor [ atKey "numbers", allItems ])
              , Tuple (accessor [ atKey "strings", allItems ]) (literal (num 42.0))
              ]
          )
          """
              {
                "numbers": [1, 2],
                "strings": ["*", "~"]
              }
            """
          [ """
              { "ciao": 1,
                "*": 42
              }
            """
          , """
              { "ciao": 1,
                "~": 42
              }
            """
          , """
              { "ciao": 2,
                "*": 42
              }
            """
          , """
              { "ciao": 2,
                "~": 42
              }
            """
          ]
    describe "Pipe" do
      it "simple pipe" do
        test (accessByIndex [ 0 ] || identity)
          """
            ["ciao", "miao"]
          """
          [ "\"ciao\"" ]
    describe "Assignment" do
      describe "update assignment (|=)" do
        it "simplest" do
          test (identity |= identity)
            """
              42
            """
            [ "42.0" ]
        it "using literal" do
          test (accessByKeyNames [ "pizza" ] |= (literal (str "Margherita")))
            """
              {
                "pizza": "Alla Diavola"
              }
            """
            [ """
              {
                "pizza": "Margherita"
              }
            """
            ]
        it "using expression" do
          test (accessByKeyNames [ "pizza" ] |= accessByIndex [ 1 ])
            """
              {
                "pizza": ["Alla Diavola", "Capricciosa"]
              }
            """
            [ """
              {
                "pizza": "Capricciosa"
              }
            """
            ]
        it "only picks the first json value if the right hand side returns more than one" do
          test (accessByKeyNames [ "pizza" ] |= accessAllItems)
            """
              {
                "pizza": ["Alla Diavola", "Capricciosa"]
              }
            """
            [ """
              {
                "pizza": "Alla Diavola"
              }
            """
            ]
        it "runs multiple updates if the left hand side returns multiple paths" do
          test ((accessByKeyNames [ "pizza" ] ~ accessByKeyNames [ "lasagna" ]) |= literal (str "great"))
            """
              {
                "pizza": "average",
                "lasagna": "mediocre"
              }
            """
            [ """
              {
                "pizza": "great",
                "lasagna": "great"
              }
            """
            ]
        it "runs multiple updates if the left hand side iterates all items (object example)" do
          test (accessAllItems |= literal (str "great"))
            """
              {
                "pizza": "average",
                "lasagna": "mediocre"
              }
            """
            [ """
              {
                "pizza": "great",
                "lasagna": "great"
              }
            """
            ]
        it "runs multiple updates if the left hand side iterates all items (array example)" do
          test (accessor [ atKey "pizza", allItems ] |= accessByKeyNames [ "tag" ])
            """
              {
                "pizza": [{"tag": "food"}, {"tag": "italian"}]
              }
            """
            [ """
              {
                "pizza": ["food", "italian"]
              }
            """
            ]
        it "creates a new key if not existing in the input object" do
          test (accessByKeyNames [ "pizza" ] |= literal (str "nice"))
            """
              { }
            """
            [ """
              {
                "pizza": "nice"
              }
            """
            ]
        it "pads the array with nulls when trying to access a non-existing item" do
          test (accessByIndex [ 2 ] |= literal (str "cheese"))
            """
              [ "Emmental" ]
            """
            [ """
              [
                "Emmental",
                null,
                "cheese"
              ]
            """
            ]
    describe "Function application" do
      it "simple identity function" do
        testWithEnv (apply "foo") (Env.fromFunction fooIdentity)
          """
            "magique"
          """
          [ "\"magique\"" ]

fooIdentity :: FunctionOptions Expression
fooIdentity = { name: "foo", arity: 0, body: identity }

test :: forall a. MonadThrow Error a => Expression -> String -> Array String -> a Unit
test expression input expectedOutput = testWithEnv expression Env.empty input expectedOutput

testWithEnv :: forall a. MonadThrow Error a => Expression -> Environment Expression -> String -> Array String -> a Unit
testWithEnv expression env input expectedOutput = case Tuple (parseJson input) (traverse parseJson expectedOutput) of
  Tuple (Right i) (Right o) -> Interpreter.run expression env [ i ] `shouldEqual` (Right o)
  _ -> fail "failed to parse JSON"
  where
  parseJson s = runParser s Json.parser

testFailure :: forall a. MonadThrow Error a => Expression -> String -> a Unit
testFailure expression input = case parseJson input of
  Right i -> case Interpreter.run expression Env.empty [ i ] of
    Left _ -> pure unit
    Right _ -> fail "this test should have seen the interpreter fail"
  _ -> fail "failed to parse JSON"
  where
  parseJson s = runParser s Json.parser

jsonInputIgnored :: String
jsonInputIgnored = "{}"
