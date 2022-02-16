module Test.PrattParser where

import Prelude (Unit, discard, (*), (+), (-), (/))
import Utils.Parsing
import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (runParser)

main :: Spec Unit
main = do
  describe "Pratt Parsing" do
    it "addition and subtraction" do
      runParser "4 + 1 - 3 + 40"
        (expressionParser { prefix: [ intParser ], infixP: [ addParser, subtractParser ] })
        `shouldEqual`
          Right (42)
    it "addition and multiplication" do
      runParser "2 * 3 + 1"
        (expressionParser { prefix: [ intParser ], infixP: [ addParser, multiplyParser ] })
        `shouldEqual`
          Right (7)
    it "left association" do
      runParser "8 / 2 / 2"
        (expressionParser { prefix: [ intParser ], infixP: [ divideParser LAssociative ] })
        `shouldEqual`
          Right (2)
    it "right association" do
      runParser "8 / 2 / 2"
        (expressionParser { prefix: [ intParser ], infixP: [ divideParser RAssociative ] })
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
