module Test.Main where

import Prelude (class Eq, class Show, Unit, bind, discard, pure, show, ($), (&&), (/=), (<$>), (<>))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.Either (Either(..))
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.String (char, satisfy)
import Text.Parsing.Parser.Combinators (choice, many1, try)
import Data.List.NonEmpty as NonEmpty
import Data.List as List
import Data.List.Types (NonEmptyList)
import Data.Foldable as Foldable
import Data.String.CodeUnits as StringStuff

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Parsing filters" do
    it "identity" do
      parse "." `shouldEqual` Right Identity

    it "select" do
      parse ".foo" `shouldEqual` Right (Select (NonEmpty.singleton "foo"))

    it "nested select" do
      parse ".foo.bar" `shouldEqual` Right (Select (NonEmpty.cons' "foo" $ List.singleton "bar"))


data Filter =
    Identity
  | Select (NonEmptyList String)

derive instance equalFilter :: Eq Filter

instance Show Filter where
  show Identity = "Identity"
  show (Select selector) = "Select " <> show selector

parse :: String -> Either ParseError Filter
parse input = runParser input filterParser

filterParser :: Parser String Filter
filterParser = do
  choice [ try select, try identity ]

select :: Parser String Filter
select = do
  steps <- many1 selectorStep
  pure $ Select (steps)

selectorStep :: Parser String String
selectorStep = do
  _ <- char '.'
  step <- Foldable.foldMap StringStuff.singleton <$> many1 selectorChars
  pure step

selectorChars :: Parser String Char
selectorChars = do
 satisfy (\c -> c /= '.' && c /= ' ' )

identity :: Parser String Filter
identity = do
  _ <- char '.'
  pure Identity

