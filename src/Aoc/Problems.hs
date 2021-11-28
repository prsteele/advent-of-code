module Aoc.Problems where

import qualified Data.Map.Strict as M
import Aoc (Solution)
import Aoc.Y2020

problems :: M.Map String (M.Map String Solution)
problems = M.fromList [("2020", problems2020)]
