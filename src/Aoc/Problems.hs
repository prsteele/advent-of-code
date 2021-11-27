module Aoc.Problems where

import qualified Data.Map.Strict as M
import Aoc (Solution)
import qualified Aoc.Y2020.P1 as P1

problems :: M.Map String Solution
problems = M.fromList [("1-2020", P1.solution)]
