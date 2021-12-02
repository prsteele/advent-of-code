module Aoc.Y2021.P1 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import qualified Data.Vector.Unboxed as V
import Text.Megaparsec
import Text.Megaparsec.Char

parser :: Parser (V.Vector Int)
parser = V.fromList <$> P.delimited P.int space <* eof

solution :: Solution
solution input = do
  v <- P.parse parser input
  solvePart1 v
  solvePart2 v

solvePart1 :: V.Vector Int -> IO ()
solvePart1 = print . countIncreases

solvePart2 :: V.Vector Int -> IO ()
solvePart2 = print . countIncreases . windowSum 3

windowSum :: Int -> V.Vector Int -> V.Vector Int
windowSum w v | n < w - 1 = V.empty
              | otherwise = V.generate (n - (w - 1)) gen
  where
    n = V.length v
    gen i = V.sum (V.slice i w v)

countIncreases :: V.Vector Int -> Int
countIncreases v
  | V.null v = 0
  | otherwise = V.sum (V.zipWith increases v (V.tail v))
  where
    increases x y =
      if x < y
        then 1
        else 0
