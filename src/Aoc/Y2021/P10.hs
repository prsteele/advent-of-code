{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2021.P10 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

type ProblemInput = Int

parser :: Parser ProblemInput
parser = undefined

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

solvePart1 :: ProblemInput -> IO Int
solvePart1 = undefined

solvePart2 :: ProblemInput -> IO Int
solvePart2 = undefined
