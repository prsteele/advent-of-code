{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P10 where

import Aoc (Solution, tshow)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Control.Monad
import Control.Monad.State
import Data.Functor (($>), (<$))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type ProblemInput = [Op]

data Op = Addx Int | Noop
  deriving (Show, Read)

parseOp :: Parser Op
parseOp = addx <|> noop
  where
    addx = Addx <$> (chunk "addx" *> hspace *> L.signed hspace P.int)
    noop = Noop <$ chunk "noop"

parser :: Parser ProblemInput
parser = P.linesOf parseOp

type ProgramState = (Int, [Int])

applyOp :: MonadState ProgramState m => Op -> m ()
applyOp Noop = recordCycle
applyOp (Addx x) = do
  -- Two clock cycles
  recordCycle
  recordCycle

  (current, history) <- get

  -- Instruction completes
  put (current + x, history)

recordCycle :: MonadState ProgramState m => m ()
recordCycle = do
  (current, history) <- get
  put (current, current : history)

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

sparsify :: Int -> Int -> [a] -> [a]
sparsify offset gap xs =
  let
   in every gap (drop offset xs)

every :: Int -> [a] -> [a]
every _ [] = []
every gap (x : rest) = x : every gap (drop (gap - 1) rest)

solvePart1 :: ProblemInput -> IO T.Text
solvePart1 ops =
  let (_, revHistory) = execState (mapM_ applyOp ops) (1, [])
      history = reverse revHistory
      strength = zipWith (*) [1 ..] history
   in pure . tshow $ sum (take 6 (sparsify 19 40 strength))

solvePart2 :: ProblemInput -> IO T.Text
solvePart2 ops =
  let (_, revHistory) = execState (mapM_ applyOp ops) (1, [])
      width = 40

      history = reverse revHistory
      withinOne clock col = abs (clock `mod` width - col) <= 1
      visible = zipWith withinOne [0 ..] history

      render [] = pure ()
      render xs =
        let next = take 40 xs
         in do
              forM_ next (\v -> putStr (if v then "#" else "."))
              putStrLn ""
              render (drop width xs)
   in render visible >> pure ""

runOn :: FilePath -> IO ()
runOn = TIO.readFile >=> solution

problem :: FilePath
problem = "data/2022/p10.txt"
