module Aoc.Y2021 where

import Aoc (Solution)
import qualified Aoc.Y2021.P1 as P1
import qualified Aoc.Y2021.P10 as P10
import qualified Aoc.Y2021.P11 as P11
import qualified Aoc.Y2021.P12 as P12
import qualified Aoc.Y2021.P13 as P13
import qualified Aoc.Y2021.P14 as P14
import qualified Aoc.Y2021.P15 as P15
import qualified Aoc.Y2021.P16 as P16
import qualified Aoc.Y2021.P17 as P17
import qualified Aoc.Y2021.P18 as P18
import qualified Aoc.Y2021.P19 as P19
import qualified Aoc.Y2021.P2 as P2
import qualified Aoc.Y2021.P20 as P20
import qualified Aoc.Y2021.P21 as P21
import qualified Aoc.Y2021.P22 as P22
import qualified Aoc.Y2021.P23 as P23
import qualified Aoc.Y2021.P24 as P24
import qualified Aoc.Y2021.P25 as P25
import qualified Aoc.Y2021.P3 as P3
import qualified Aoc.Y2021.P4 as P4
import qualified Aoc.Y2021.P5 as P5
import qualified Aoc.Y2021.P6 as P6
import qualified Aoc.Y2021.P7 as P7
import qualified Aoc.Y2021.P8 as P8
import qualified Aoc.Y2021.P9 as P9
import qualified Data.Map.Strict as M

problems2021 :: M.Map String Solution
problems2021 =
  M.fromList
    [ ("1", P1.solution),
      ("2", P2.solution),
      ("3", P3.solution),
      ("4", P4.solution),
      ("5", P5.solution),
      ("6", P6.solution),
      ("7", P7.solution),
      ("8", P8.solution),
      ("9", P9.solution),
      ("10", P10.solution),
      ("11", P11.solution),
      ("12", P12.solution),
      ("13", P13.solution),
      ("14", P14.solution),
      ("15", P15.solution),
      ("16", P16.solution),
      ("17", P17.solution),
      ("18", P18.solution),
      ("19", P19.solution),
      ("20", P20.solution),
      ("21", P21.solution),
      ("22", P22.solution),
      ("23", P23.solution),
      ("24", P24.solution),
      ("25", P25.solution)
    ]
