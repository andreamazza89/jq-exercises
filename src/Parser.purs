module Parser where

import Prelude (bind, pure, ($), (&&), (/=), (<$>))

import Data.Either (Either)
import Text.Parsing.Parser (ParseError, runParser, Parser)
import Text.Parsing.Parser.Combinators (choice, many1, try)
import Text.Parsing.Parser.String (char, satisfy)
import Filter (Filter(..))
import Data.String.CodeUnits as StringStuff
import Data.Foldable as Foldable

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
