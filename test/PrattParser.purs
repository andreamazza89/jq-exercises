module Test.PrattParser where

import Prelude (Unit, discard, map, (#), (*), (+), (-), (/), (>>>))
import Utils.Parsing
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Int (fromString)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.String.CodeUnits (singleton)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (many1)

main :: Spec Unit
main = do
  describe "Pratt Parsing" do
    it "addition and subtraction" do
      runParser "4 + 1 - 3 + 40"
        (expressionParser { prefix: [ intParser ], infix: [ addParser, subtractParser ] })
        `shouldEqual`
          Right (42)
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

addParser :: InfixParser Int
addParser = infixLeft "+" 1 (+)

subtractParser :: InfixParser Int
subtractParser = infixLeft "-" 1 (-)

multiplyParser :: InfixParser Int
multiplyParser = infixLeft "*" 2 (*)

divideParser :: Associativity -> InfixParser Int
divideParser LAssociative = infixLeft "/" 3 (/)
divideParser RAssociative = infixRight "/" 3 (/)

charsToString :: NonEmptyList Char -> String
charsToString = Foldable.foldMap singleton

charsToInt :: NonEmptyList Char -> Maybe Int
charsToInt = charsToString >>> fromString

intParser :: Parser String Int
intParser =
  many1 digit
    # map charsToInt
    # required
