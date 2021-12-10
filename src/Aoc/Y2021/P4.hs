{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Aoc.Y2021.P4 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Data.Functor
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tuple
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

data Input = Input [Int] [Board]
  deriving (Show)

data Board = Board (M.Map (Int, Int) Int) (S.Set (Int, Int))
  deriving (Show)

parser :: Parser Input
parser =
  Input
    <$> randomNums
    <* some newline
    <*> sepBy bingoBoard space1
    <* space
    <* eof

bingoRow :: Parser [Int]
bingoRow = hspace *> sepBy1 P.int hspace <* hspace

bingoBoard :: Parser Board
bingoBoard = f <$> sepEndBy bingoRow (hspace *> newline)
  where
    f rows = Board (board rows) S.empty

    board rows =
      M.fromList $
        [ ((i, j), k)
          | (i, row) <- enumerate rows,
            (j, k) <- enumerate row
        ]

    enumerate = zip [1 ..]

randomNums :: Parser [Int]
randomNums = sepBy P.int (string ",")

play :: Int -> Board -> Board
play num (Board board marked) = Board board (S.union marked matches)
  where
    matches :: S.Set (Int, Int)
    matches = M.keysSet (M.filter (== num) board)

rowsAndCols :: [S.Set (Int, Int)]
rowsAndCols = rows ++ cols
  where
    rows = [S.fromList [(i, j) | j <- [1 .. 5]] | i <- [1 .. 5]]
    cols = S.map swap <$> rows

isWinner :: Board -> Bool
isWinner (Board _ marked) = any (`S.isSubsetOf` marked) rowsAndCols

unmarked :: Board -> [Int]
unmarked (Board board marked) = M.elems (M.withoutKeys board marked)

findWinners :: Input -> [(Board, Int)]
findWinners (Input nums' boards') = go nums' boards'
  where
    go [] _ = []
    go (n : ns) boards =
      if null winners
        then go ns markedBoards
        else fmap (,n) winners
      where
        markedBoards = play n <$> boards
        winners = filter isWinner markedBoards

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

solvePart1 :: Input -> IO Int
solvePart1 input = case findWinners input of
  ((w, n) : _) -> pure (n * sum (unmarked w))
  [] -> putStrLn "Didn't find a winner" >> pure 0

solvePart2 :: Input -> IO Int
solvePart2 = undefined

sample :: T.Text
sample =
  "\
  \7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
  \\n\
  \22 13 17 11  0\n\
  \ 8  2 23  4 24\n\
  \21  9 14 16  7\n\
  \ 6 10  3 18  5\n\
  \ 1 12 20 15 19\n\
  \\n\
  \ 3 15  0  2 22\n\
  \ 9 18 13 17  5\n\
  \19  8  7 25 23\n\
  \20 11 10 24  4\n\
  \14 21 16 12  6\n\
  \\n\
  \14 21 17 24  4\n\
  \10 16 15  9 19\n\
  \18  8 23 26 20\n\
  \22 11 13  6  5\n\
  \ 2  0 12  3  7"
