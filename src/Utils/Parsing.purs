module Utils.Parsing where

import Data.Array (fromFoldable) as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.Maybe (Maybe, maybe)
import Data.String.CodePoints (codePointFromChar)
import Prelude (class Eq, bind, discard, map, pure, (#), ($), (-), (<), (==), (>>>))
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, choice, lookAhead, optionMaybe, sepBy)
import Text.Parsing.Parser.String (char, satisfy, skipSpaces, string)

comma :: Parser String Char
comma = char ','

dash :: Parser String Char
dash = (char '-')

digit :: Parser String Char
digit = satisfy (codePointFromChar >>> isDecDigit)

dot :: Parser String Char
dot = char '.'

quoted :: forall a. Parser String a -> Parser String a
quoted = between (char '"') (char '"')

required :: forall a. Parser String (Maybe a) -> Parser String a
required maybeParser = do
  a <- maybeParser
  maybe (fail "value must exist") pure a

sepByCommas :: forall a. Parser String a -> Parser String (Array a)
sepByCommas p =
  sepBy p (spaced $ comma)
    # map Array.fromFoldable

spaced :: forall a. Parser String a -> Parser String a
spaced p = do
  skipSpaces
  value <- p
  skipSpaces
  pure value

inCurlies :: forall a. Parser String a -> Parser String a
inCurlies = between openCurly closeCurly

openCurly :: Parser String Char
openCurly = spaced $ char '{'

closeCurly :: Parser String Char
closeCurly = spaced $ char '}'

inSquares :: forall a. Parser String a -> Parser String a
inSquares = between openSquare closeSquare

openSquare :: Parser String Char
openSquare = spaced $ char '['

closeSquare :: Parser String Char
closeSquare = spaced $ char ']'

-- Parsing expressions with a Pratt Parser - this blog post really helped me figure it out:
-- http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
type Precedence
  = Int

data Associativity
  = LAssociative
  | RAssociative

derive instance equalAssociativity :: Eq Associativity

type InfixParser a
  = Parser String 
      { prec :: Precedence
      , buildExp :: a -> a -> a
      , associativity :: Associativity
      }

expressionParser ::
  forall a.
  { prefix :: Array (Parser String a)
  , infix :: Array (InfixParser a)
  } ->
  Parser String a
expressionParser input = parser 0
  where
  parser pr = do
    left <- choice input.prefix
    loop pr left

  loop currentPrecedence left = do
    infx <- optionMaybe $ lookAhead $ choice input.infix
    maybe
      (pure left)
      ( \fx ->
          if currentPrecedence < fx.prec then do
            mx <- choice input.infix
            rExp <- parser (if mx.associativity == LAssociative then mx.prec else mx.prec - 1)
            loop currentPrecedence (mx.buildExp left rExp)
          else
            pure left
      )
      infx

infixLeft :: forall exp . String -> Precedence -> (exp -> exp -> exp) -> InfixParser exp
infixLeft  operator precedence buildExpression = do
  _ <- spaced $ string operator
  pure
    { prec: precedence
    , buildExp: buildExpression
    , associativity: LAssociative
    }

infixRight :: forall exp . String -> Precedence -> (exp -> exp -> exp) -> InfixParser exp
infixRight  operator precedence buildExpression = do
  _ <- spaced $ string operator
  pure
    { prec: precedence
    , buildExp: buildExpression
    , associativity: RAssociative
    }
