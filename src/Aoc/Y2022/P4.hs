{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P4 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

type ProblemInput = [(Range, Range)]

type Range = (Int, Int)

parsePair :: Parser (Range, Range)
parsePair = do
  a <- P.int
  chunk "-"
  b <- P.int
  chunk ","
  c <- P.int
  chunk "-"
  d <- P.int
  pure ((a, b), (c, d))

contains :: (Range, Range) -> Bool
contains ((a, b), (c, d)) = (a <= c && d <= b) || (c <= a && b <= d)

overlap :: (Range, Range) -> Bool
overlap ((a, b), (c, d)) = max a c <= min b d

parser :: Parser ProblemInput
parser = P.linesOf parsePair

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

solvePart1 :: ProblemInput -> IO Int
solvePart1 = pure . length . filter contains

solvePart2 :: ProblemInput -> IO Int
solvePart2 = pure . length . filter overlap
