{-# LANGUAGE TypeFamilies #-}

module Aoc.Y2020.P1 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Aoc.Sort (mutInit, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Text.Megaparsec.Char (space)

parser :: Parser (V.Vector Int)
parser = V.fromList <$> P.delimited P.int space

solution :: Solution
solution input = P.parse parser input >>= solve >>= print

solve :: V.Vector Int -> IO Int
solve v = do
  mv <- mutInit v
  sort mv
  fromMaybe (error "Failed to find sum") <$> search mv 2020

search :: M.IOVector Int -> Int -> IO (Maybe Int)
search v target =
  let search' i j
        | i >= j = pure Nothing
        | otherwise = do
          ix <- M.read v i
          jx <- M.read v j
          case compare (ix + jx) target of
            LT -> search' (i + 1) j
            EQ -> pure (pure (ix * jx))
            GT -> search' i (j - 1)
   in search' 0 (M.length v - 1)
