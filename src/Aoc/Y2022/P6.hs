{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P6 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

type ProblemInput = T.Text

parser :: Parser ProblemInput
parser = P.word

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

solvePart1 :: ProblemInput -> IO T.Text
solvePart1 input =
  let len = T.length input
      n = 4
      allDiff i = S.size (S.fromList [T.index input j | j <- fmap (min (len - 1)) [i .. i + n -1]]) == n
   in do
        print (n + fst (head (filter snd (zip [0 ..] (fmap allDiff [0 ..])))))
        pure ""

solvePart2 :: ProblemInput -> IO T.Text
solvePart2 input =
  let len = T.length input
      n = 14
      allDiff i = S.size (S.fromList [T.index input j | j <- fmap (min (len - 1)) [i .. i + n -1]]) == n
   in do
        print (n + fst (head (filter snd (zip [0 ..] (fmap allDiff [0 ..])))))
        pure ""

runOn :: FilePath -> IO ()
runOn = TIO.readFile >=> solution

p6 :: FilePath
p6 = "/home/prsteele/Documents/advent-of-code/data/2022/p6.txt"
