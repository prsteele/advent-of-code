{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P2 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

data Cypher = A | B | C | X | Y | Z
  deriving (Show, Ord, Eq)

data RPS = R | P | S
  deriving (Show, Ord, Eq)

data Outcome = W | L | D
  deriving (Show, Ord, Eq)

type ProblemInput = V.Vector (Cypher, Cypher)

parseCypher :: Parser Cypher
parseCypher =
  chunk "A" $> A
    <|> chunk "B" $> B
    <|> chunk "C" $> C
    <|> chunk "X" $> X
    <|> chunk "Y" $> Y
    <|> chunk "Z" $> Z

parseLine :: Parser (Cypher, Cypher)
parseLine = (,) <$> parseCypher <*> (space *> parseCypher)

parser :: Parser ProblemInput
parser = V.fromList <$> sepEndBy parseLine eol <* eof

score :: (RPS, RPS) -> Int
score (x, y) = playScore y + gameScore x y

playScore :: RPS -> Int
playScore R = 1
playScore P = 2
playScore S = 3

gameScore :: RPS -> RPS -> Int
gameScore R P = 6
gameScore P S = 6
gameScore S R = 6
gameScore P P = 3
gameScore R R = 3
gameScore S S = 3
gameScore _ _ = 0

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

solvePart1 :: ProblemInput -> IO Int
solvePart1 = pure . V.sum . fmap (score . convert)
  where
    convert (x, y) = (f x, f y)
    f A = R
    f B = P
    f C = S
    f X = R
    f Y = P
    f Z = S

beats :: RPS -> RPS
beats R = P
beats S = R
beats P = S

loses :: RPS -> RPS
loses = beats . beats

draws :: RPS -> RPS
draws = id

solvePart2 :: ProblemInput -> IO Int
solvePart2 = pure . V.sum . fmap (score . convert)
  where
    convert (x, X) = (f x, loses (f x))
    convert (x, Y) = (f x, draws (f x))
    convert (x, Z) = (f x, beats (f x))
    convert _ = error "Bad line"

    f A = R
    f B = P
    f C = S
    f _ = error "Bad cypher"
