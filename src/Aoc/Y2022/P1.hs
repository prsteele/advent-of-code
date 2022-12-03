{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P1 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import Text.Megaparsec
import Text.Megaparsec.Char

type ProblemInput = V.Vector (V.Vector Int)

countGroup :: Parser (V.Vector Int)
countGroup = V.fromList <$> sepEndBy P.int eol

parser :: Parser ProblemInput
parser = V.fromList <$> sepEndBy countGroup eol <* eof

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

calsPerElf :: ProblemInput -> V.Vector Int
calsPerElf = fmap V.sum

solvePart1 :: ProblemInput -> IO Int
solvePart1 = pure . maximum . calsPerElf

solvePart2 :: ProblemInput -> IO Int
solvePart2 cals =
  let ordered = V.modify I.sort (calsPerElf cals)
   in pure $ V.sum (V.slice (V.length cals - 3) 3 ordered)
