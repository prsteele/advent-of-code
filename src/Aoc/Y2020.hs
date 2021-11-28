module Aoc.Y2020 where
import Aoc (Solution)
import qualified Data.Map.Strict as M
import qualified Aoc.Y2020.P1 as P1

problems2020 :: M.Map String Solution
problems2020 = M.fromList [ ("1", P1.solution)
                          ]
