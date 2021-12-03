module Aoc.Y2021 where

import Aoc (Solution)
import qualified Data.Map.Strict as M
import qualified Aoc.Y2021.P1 as P1
import qualified Aoc.Y2021.P2 as P2

problems2021 :: M.Map String Solution
problems2021 =
  M.fromList
    [("1", P1.solution), ("2", P2.solution)]
