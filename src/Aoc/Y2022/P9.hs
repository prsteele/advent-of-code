{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P9 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Control.Monad
import Control.Monad.State
import Data.Functor (($>))
import Data.IntMap (difference)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

data Direction = U | D | L | R
  deriving
    (Show, Read)

data Move = Move Direction Int
  deriving
    (Show, Read)

parseDirection :: Parser Direction
parseDirection =
  chunk "U" $> U
    <|> chunk "D" $> D
    <|> chunk "L" $> L
    <|> chunk "R" $> R

parseMove :: Parser Move
parseMove = Move <$> parseDirection <*> (hspace *> P.int)

type ProblemInput = [Move]

parser :: Parser ProblemInput
parser = P.linesOf parseMove

type Coordinate = (Int, Int)

data Knot = Knot Coordinate Coordinate
  deriving (Show, Read)

moveCoordinate :: Coordinate -> Direction -> Coordinate
moveCoordinate (x, y) U = (x, y + 1)
moveCoordinate (x, y) D = (x, y -1)
moveCoordinate (x, y) L = (x -1, y)
moveCoordinate (x, y) R = (x + 1, y)

isDiagonal :: Knot -> Bool
isDiagonal (Knot (hx, hy) (tx, ty)) = hx /= tx && hy /= ty

isOverlapping :: Knot -> Bool
isOverlapping (Knot h t) = h == t

isApart :: Knot -> Bool
isApart (Knot (hx, hy) (tx, ty)) = max (abs (hx - tx)) (abs (hy - ty)) > 1

move' :: Knot -> Direction -> Knot
move' knot@(Knot h t) direction
  | isOverlapping knot = Knot h' t
  | isDiagonal knot =
    if isApart (Knot h' t)
      then Knot h' h
      else Knot h' t
  | otherwise =
    if isApart (Knot h' t)
      then Knot h' h
      else Knot h' t
  where
    h' = moveCoordinate h direction

type KnotState = (Knot, [Coordinate])

move :: MonadState KnotState m => Direction -> m ()
move direction = do
  (knot, record) <- get
  put (move' knot direction, record)

recordTail :: MonadState KnotState m => m ()
recordTail = do
  (knot@(Knot _ t), record) <- get
  put (knot, t : record)

followMove :: MonadState KnotState m => Move -> m ()
followMove (Move direction len) =
  replicateM_ len (move direction >> recordTail)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

solvePart1 :: ProblemInput -> IO T.Text
solvePart1 moves =
  let (_, record) = execState (mapM_ followMove moves) (Knot (0, 0) (0, 0), [])
      unique = S.fromList record
   in pure . tshow . S.size $ unique

solvePart2 :: ProblemInput -> IO T.Text
solvePart2 _ = pure ("Not yet implemented")

runOn :: FilePath -> IO ()
runOn = TIO.readFile >=> solution

problem :: FilePath
problem = "data/2022/p9.txt"
