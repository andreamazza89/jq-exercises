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

    it "left association" do
      runParser "8 / 2 / 2"
        (expressionParser { prefix: [ intParser ], infix: [ divideParser LAssociative ] })
        `shouldEqual`
          Right (2)

    it "right association" do
      runParser "8 / 2 / 2"
        (expressionParser { prefix: [ intParser ], infix: [ divideParser RAssociative ] })
        `shouldEqual`
          Right (8)


addParser :: (Int -> Parser String Int) -> Int -> Parser String (Infix Int)
addParser p exp = do
  _ <- spaced $ char '+'
  rExp <- p 1
  pure { exp : exp + rExp
    , precedence : 1
    , associativity : LAssociative
    }

subtractParser :: (Int -> Parser String Int) -> Int -> Parser String (Infix Int)
subtractParser p exp = do
  _ <- spaced $ char '-'
  rExp <- p 1
  pure { exp : exp - rExp
    , precedence : 1
    , associativity : LAssociative
    }

multiplyParser :: (Int -> Parser String Int) -> Int -> Parser String (Infix Int)
multiplyParser p exp = do
  _ <- spaced $ char '*'
  rExp <- p 2
  pure { exp : exp * rExp
    , precedence : 2
    , associativity : LAssociative
    }

divideParser :: Associativity -> (Int -> Parser String Int) -> Int -> Parser String (Infix Int)
divideParser associativity p exp = do
  _ <- spaced $ char '/'
  rExp <- if associativity == LAssociative then p 3 else p 2
  pure { exp : exp / rExp
    , precedence : 3
    , associativity : associativity
    }


charsToString :: NonEmptyList Char -> String
charsToString = Foldable.foldMap singleton

charsToInt :: NonEmptyList Char -> Maybe Int
charsToInt = charsToString >>> fromString

intParser :: Parser String Int
intParser =
  many1 digit
    # map charsToInt
    # required
