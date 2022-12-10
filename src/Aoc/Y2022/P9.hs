{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P9 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Control.Monad
import Control.Monad.State
import Data.Functor (($>))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

data Direction = U | D | L | R | UR | UL | DR | DL | Z
  deriving
    (Eq, Show, Read)

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

type Knot = (Int, Int)

type Rope = [Knot]

type KnotPair = (Knot, Knot)

moveKnot :: Knot -> Direction -> Knot
moveKnot (x, y) U = (x, y + 1)
moveKnot (x, y) D = (x, y -1)
moveKnot (x, y) L = (x -1, y)
moveKnot (x, y) R = (x + 1, y)
moveKnot c UR = (`moveKnot` U) . (`moveKnot` R) $ c
moveKnot c UL = (`moveKnot` U) . (`moveKnot` L) $ c
moveKnot c DR = (`moveKnot` D) . (`moveKnot` R) $ c
moveKnot c DL = (`moveKnot` D) . (`moveKnot` L) $ c
moveKnot c Z = c

adjacent :: Knot -> Knot -> Bool
adjacent (hx, hy) (tx, ty) = max (abs (hx - tx)) (abs (hy - ty)) <= 1

-- If the head moves in a direction, how should the tail move?
moveTail :: KnotPair -> Knot
moveTail (h@(hx, hy), t@(tx, ty))
  -- If the new head is still adjacent, the tail doesn't move
  | adjacent h t = t
  -- Chase the head
  | otherwise = moveKnot t d
  where
    d = case (compare tx hx, compare ty hy) of
      (LT, LT) -> UR
      (LT, EQ) -> R
      (LT, GT) -> DR
      (EQ, LT) -> U
      (EQ, EQ) -> Z
      (EQ, GT) -> D
      (GT, LT) -> UL
      (GT, EQ) -> L
      (GT, GT) -> DL

move' :: Rope -> Direction -> Rope
move' [] _ = []
move' [h] d = [moveKnot h d]
move' rope@(origHead : _) d = chase (moveKnot (moveKnot origHead d) d) rope
  where
    chase _ [] = []
    chase h (t : rest) = t' : chase t' rest
      where
        t' = moveTail (h, t)

type RopeState = (Rope, S.Set Knot)

move :: MonadState RopeState m => Direction -> m ()
move direction = do
  (rope, visited) <- get
  put (move' rope direction, visited)

recordTail :: MonadState RopeState m => m ()
recordTail = do
  (rope, visited) <- get
  put (rope, S.insert (last rope) visited)

followMove :: MonadState RopeState m => Move -> m ()
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
  let (_, visited) = execState (mapM_ followMove moves) (replicate 2 (0, 0), S.singleton (0, 0))
   in pure . tshow . S.size $ visited

solvePart2 :: ProblemInput -> IO T.Text
solvePart2 moves =
  let (_, visited) = execState (mapM_ followMove moves) (replicate 10 (0, 0), S.singleton (0, 0))
   in pure . tshow . S.size $ visited

runOn :: FilePath -> IO ()
runOn = TIO.readFile >=> solution

problem :: FilePath
problem = "data/2022/p9.txt"
