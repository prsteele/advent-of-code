{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P8 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Forest = Forest Int Int (M.Map (Int, Int) Int)
  deriving (Show, Read)

type ProblemInput = Forest

parser :: Parser ProblemInput
parser = do
  ixs <- P.indexed P.singleDigit
  let rows = 1 + maximum ((-1) : fmap fst (M.keys ixs))
      cols = 1 + maximum ((-1) : fmap snd (M.keys ixs))

  pure (Forest rows cols ixs)

rowIxs :: Int -> Forest -> [(Int, Int)]
rowIxs row (Forest _ cols _) = [(row, col) | col <- [0 .. cols -1]]

colIxs :: Int -> Forest -> [(Int, Int)]
colIxs col (Forest rows _ _) = [(row, col) | row <- [0 .. rows -1]]

height :: Forest -> (Int, Int) -> Int
height (Forest _ _ grid) ix = M.findWithDefault 0 ix grid

visible :: [Int] -> [Bool]
visible xs = zipWith (||) (heighestSoFar xs) (reverse (heighestSoFar (reverse xs)))

heighestSoFar :: [Int] -> [Bool]
heighestSoFar [] = []
heighestSoFar (x : xs) = True : go x xs
  where
    go m (z : zs) = (z > m) : go (max m z) zs
    go _ [] = []

upTo :: Int -> Int -> [Int]
upTo from to
  | from <= to = [from .. to]
  | otherwise = []

downTo :: Int -> Int -> [Int]
downTo from to
  | from >= to = [from, from - 1 .. to]
  | otherwise = []

scenicScore :: (Int, Int) -> Forest -> Int
scenicScore ix@(r, c) forest@(Forest rows cols _) =
  let h = height forest ix
      ups = [(r', c) | r' <- (r - 1) `downTo` 0]
      downs = [(r', c) | r' <- (r + 1) `upTo` (rows - 1)]
      lefts = [(r, c') | c' <- (c - 1) `downTo` 0]
      rights = [(r, c') | c' <- (c + 1) `upTo` (cols - 1)]

      sees = length . takeUntilFirst (>= h)
   in product (fmap (sees . fmap (height forest)) [ups, downs, lefts, rights])

takeUntilFirst :: (a -> Bool) -> [a] -> [a]
takeUntilFirst _ [] = []
takeUntilFirst f (x : xs)
  | f x = [x]
  | otherwise = x : takeUntilFirst f xs

tshow :: Show a => a -> T.Text
tshow = T.pack . show

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

solvePart1 :: ProblemInput -> IO T.Text
solvePart1 forest@(Forest rows cols _) =
  let allVisible = S.unions (fmap filterIxs (rowIxSet ++ colIxSet))
      filterIxs :: [(Int, Int)] -> S.Set (Int, Int)
      filterIxs ixs =
        let heights = fmap (height forest) ixs
            visibility = zip ixs (visible heights)
         in S.fromList . fmap fst . filter snd $ visibility

      rowIxSet = [rowIxs r forest | r <- [0 .. rows -1]]
      colIxSet = [colIxs c forest | c <- [0 .. cols -1]]
   in pure . tshow $ S.size allVisible

solvePart2 :: ProblemInput -> IO T.Text
solvePart2 forest@(Forest _ _ grid) =
  let scores = M.mapWithKey (\k _ -> scenicScore k forest) grid
   in pure . tshow . maximum $ M.elems scores

runOn :: FilePath -> IO ()
runOn = TIO.readFile >=> solution

p8 :: FilePath
p8 = "/home/prsteele/Documents/advent-of-code/data/2022/p8.txt"
