module Test.PrattParser where

import Prelude
import Utils.Parsing

import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Int (fromString)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe)
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (many1)
import Text.Parsing.Parser.String (char)

main :: Spec Unit
main = do
  describe "Pratt Parsing" do
    it "addition and subtraction" do
      runParser "4 + 1 - 3"
        (expressionParser { prefix: [ intParser ], infix: [ addParser, subtractParser ] })
        `shouldEqual`
          Right (2)
    it "addition and multiplication" do
      runParser "2 * 3 + 1"
        (expressionParser { prefix: [ intParser ], infix: [ addParser, multiplyParser ] })
        `shouldEqual`
          Right (7)

addParser :: Parser String Int -> Int -> Parser String (Tuple Int Int)
addParser p exp = do
  _ <- spaced $ char '+'
  rExp <- p
  pure (Tuple 1 $ exp + rExp)

subtractParser :: Parser String Int -> Int -> Parser String (Tuple Int Int)
subtractParser p exp = do
  _ <- spaced $ char '-'
  rExp <- p
  pure (Tuple 1 $ exp - rExp)

multiplyParser :: Parser String Int -> Int -> Parser String (Tuple Int Int)

multiplyParser p exp = do
  _ <- spaced $ char '*'
  rExp <- p
  pure (Tuple 2 $ exp * rExp)

charsToString :: NonEmptyList Char -> String
charsToString = Foldable.foldMap singleton

charsToInt :: NonEmptyList Char -> Maybe Int
charsToInt = charsToString >>> fromString

intParser :: Parser String Int
intParser =
  many1 digit
    # map charsToInt
    # required
