{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P2 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Control.Monad
import Control.Monad.State
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

runOn :: FilePath -> IO ()
runOn = TIO.readFile >=> solution

problem :: FilePath
problem = "data/2022/p2.txt"
