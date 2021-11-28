{-# LANGUAGE TypeFamilies #-}

module Aoc.Y2020.P1 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Aoc.Sort (mutInit, sort)
import qualified Data.Vector.Unboxed as V
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (space)

parser :: Parser (V.Vector Int)
parser = V.fromList <$> P.delimited P.int space <* eof

solution :: Solution
solution input = do
  mv <- P.parse parser input >>= mutInit
  sort mv
  v <- V.unsafeFreeze mv
  solvePart1 2020 v
  solvePart2 2020 v

solvePart1 :: Int -> V.Vector Int -> IO ()
solvePart1 target v =
  case twoSum v target of
    Nothing -> putStrLn "Part 1: Failed to find sum"
    Just x -> print x

solvePart2 :: Int -> V.Vector Int -> IO ()
solvePart2 target v =
  case threeSum v target of
    Nothing -> putStrLn "Part 2: Failed to find sum"
    Just x -> print x

twoSum :: V.Vector Int -> Int -> Maybe Int
twoSum v target =
  let search i j
        | i >= j = Nothing
        | otherwise =
          let ix = v V.! i
              jx = v V.! j
           in case compare (ix + jx) target of
                LT -> search (i + 1) j
                EQ -> Just (ix * jx)
                GT -> search i (j - 1)
   in search 0 (V.length v - 1)

threeSum :: V.Vector Int -> Int -> Maybe Int
threeSum v target =
  let len = V.length v
      search i
        | i >= len - 2 = Nothing
        | otherwise =
          case twoSum (V.slice (i + 1) (len - i - 1) v) (target - v V.! i) of
            Just x -> Just $ (v V.! i) * x
            Nothing -> search (i + 1)
   in search 0
