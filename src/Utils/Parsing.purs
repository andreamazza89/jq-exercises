module Utils.Parsing where

import Data.Array (fromFoldable) as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.Foldable as Foldable
import Data.Int (fromString) as Int
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (singleton) as String
import Prelude (bind, discard, map, pure, (#), ($), (-), (<), (>>>))
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, choice, lookAhead, many1, optionMaybe, sepBy)
import Text.Parsing.Parser.String (char, satisfy, skipSpaces, string)

-- Single character
comma :: Parser String Char
comma = char ','

colon :: Parser String Char
colon = char ':'


dash :: Parser String Char
dash = (char '-')

digit :: Parser String Char
digit = satisfy (codePointFromChar >>> isDecDigit)

dot :: Parser String Char
dot = char '.'

-- Parser Modifiers
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

-- Int

intParser :: Parser String Int
intParser =
  many1 digit
    # map charsToInt
    # required

charsToInt :: NonEmptyList Char -> Maybe Int
charsToInt = charsToString >>> Int.fromString

charsToString :: NonEmptyList Char -> String
charsToString = Foldable.foldMap String.singleton


-- Parsing expressions with a Pratt Parser - this blog post really helped me figure it out:
-- http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
type ParserConfig exp =
  { prefix :: Array (Parser String exp)
  , infixP :: Array (InfixParser exp)
  }

type InfixParser exp
  = Parser String (InfixConfig exp)

type InfixConfig exp =
  { prec :: Precedence
  , buildExp :: exp -> exp -> exp
  , associativity :: Associativity
  }

type Precedence
  = Int

data Associativity
  = LAssociative
  | RAssociative

expressionParser :: forall exp.  ParserConfig exp -> Parser String exp
expressionParser =
  expressionParser_ 0
expressionParser_ :: forall exp.  Precedence -> ParserConfig exp -> Parser String exp
expressionParser_ precedence input = do
    leftExp <- spaced $ choice input.prefix
    loop precedence leftExp input

loop :: forall exp. Precedence -> exp -> ParserConfig exp -> Parser String exp
loop precedence leftExp input = do
  infixConfig <- optionMaybe $ lookAhead $ choice input.infixP
  infixConfig
    # map parseInfix
    # fromMaybe (pure leftExp)
  where
    parseInfix infixConfig = 
      if precedence < infixConfig.prec then do
        _ <- choice input.infixP
        rightExp <- expressionParser_ (getPrecedence infixConfig) input
        loop precedence (infixConfig.buildExp leftExp rightExp) input
      else
        pure leftExp
    getPrecedence infixConfig =
      case infixConfig.associativity of
        LAssociative -> infixConfig.prec
        RAssociative -> infixConfig.prec - 1
    

infixLeft :: forall exp. String -> Precedence -> (exp -> exp -> exp) -> InfixParser exp
infixLeft operator precedence buildExpression = do
  _ <- string operator
  pure
    { prec: precedence
    , buildExp: buildExpression
    , associativity: LAssociative
    }

infixRight :: forall exp. String -> Precedence -> (exp -> exp -> exp) -> InfixParser exp
infixRight operator precedence buildExpression = do
  _ <- string operator
  pure
    { prec: precedence
    , buildExp: buildExpression
    , associativity: RAssociative
    }
