module Utils.Parsing where

import Data.Array (fromFoldable) as Array
import Data.CodePoint.Unicode (isDecDigit)
import Data.Functor (map)
import Data.Maybe (Maybe, maybe)
import Data.String.CodePoints (codePointFromChar)
import Prelude (bind, discard, pure, ($), (#), (>>>))
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, sepBy)
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
