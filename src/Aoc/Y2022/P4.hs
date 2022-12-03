{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P4 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import qualified Data.Text as T
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

solvePart1 :: ProblemInput -> IO T.Text
solvePart1 _ = pure ("Not yet implemented")

solvePart2 :: ProblemInput -> IO T.Text
solvePart2 _ = pure ("Not yet implemented")
