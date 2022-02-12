module Utils.Parsing where

import Control.Lazy (fix)
import Data.Array (fromFoldable) as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.Functor (map)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Eq, bind, discard, map, pure, (#), ($), (-), (<), (==), (>>>))
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, choice, lookAhead, option, optionMaybe, sepBy, try)
import Text.Parsing.Parser.String (char, satisfy, skipSpaces)

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

-- Parsing expressions with a Pratt Parser
type Precedence
  = Int

data Associativity
  = LAssociative
  | RAssociative

derive instance equalAssociativity :: Eq Associativity

type Infix a
  = { exp :: a
    , precedence :: Precedence
    , associativity :: Associativity
    }

precccc :: forall a. Infix a -> Int
precccc inffiix = case inffiix.associativity of
  LAssociative -> inffiix.precedence
  RAssociative -> inffiix.precedence - 1

expressionParser ::
  forall a.
  { prefix :: Array (Parser String a)
  , infix :: Array ((Precedence -> Parser String a) -> a -> Parser String (Infix a))
  } ->
  Parser String a
expressionParser input = parser 0
  where
  parser prec = do
    left <- choice input.prefix
    loop prec left

  loop currentPrecedence left = do
    infx <- optionMaybe $ lookAhead $ choice (map (\i -> try $ i parser left) input.infix)
    maybe
      (pure left)
      ( \fx ->
          if currentPrecedence < fx.precedence then do
            newL <- (choice (map (\i -> try $ i parser left) input.infix))
            loop currentPrecedence newL.exp
          else
            pure left
      )
      infx

type InfixParser a
  = Parser String ({ prec :: Precedence, buildExp :: a -> a -> a, associativity :: Associativity })

expressionParser2 ::
  forall a.
  { prefix :: Array (Parser String a)
  , infix :: Array (InfixParser a)
  } ->
  Parser String a
expressionParser2 input = parser 0
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
