module Aoc.Problems where

import Aoc (Solution)
import Aoc.Y2020
import Aoc.Y2021
import qualified Data.Map.Strict as M

problems :: M.Map String (M.Map String Solution)
problems = M.fromList [("2020", problems2020), ("2021", problems2021)]
