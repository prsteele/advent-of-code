module Aoc.Parsers where

import qualified Data.Text as T
import Data.Void
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

parseText :: Parser a -> T.Text -> Either (ParseErrorBundle T.Text Void) a
parseText p = runParser p "<string>"

parse :: Parser a -> T.Text -> IO a
parse p x = case parseText p x of
  Left err -> print err >> exitFailure
  Right result -> pure result

int :: Parser Int
int = L.decimal

integer :: Parser Integer
integer = L.decimal

double :: Parser Double
double = L.float

word :: Parser T.Text
word = T.pack <$> many alphaNumChar

delimited :: Parser a -> Parser b -> Parser [a]
delimited p delim = Text.Megaparsec.Char.space *> sepEndBy p (delim *> Text.Megaparsec.Char.space)

spaced :: Parser a -> Parser [a]
spaced p = Text.Megaparsec.Char.space *> sepEndBy p Text.Megaparsec.Char.space
