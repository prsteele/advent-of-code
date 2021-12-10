{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Aoc.Y2021.P3 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Data.Functor
import Data.Functor.Foldable
import qualified Data.Text as T
import Data.Tuple.Extra
import Text.Megaparsec
import Text.Megaparsec.Char

type ProblemInput = Int

parser :: Parser [[Bit]]
parser = parseReport

data Bit = B0 | B1
  deriving (Eq, Show)

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

bitCounts :: [Bit] -> (Int, Int)
bitCounts = cata go
  where
    go Nil = (0, 0)
    go (Cons B0 (x, y)) = (x + 1, y)
    go (Cons B1 (x, y)) = (x, y + 1)

powerConsumption :: [[Bit]] -> Int
powerConsumption = uncurry (*) . gammaEpsilon

mostCommon :: [[Bit]] -> [Bit]
mostCommon = fmap vote . cata go
  where
    f B0 (i, j) = (i + 1, j)
    f B1 (i, j) = (i, j + 1)

    vote :: (Int, Int) -> Bit
    vote (i, j)
      | j >= i = B1
      | otherwise = B0

    go Nil = repeat (0, 0)
    go (Cons bits rest) = zipWith f bits rest

flipBit :: Bit -> Bit
flipBit B0 = B1
flipBit B1 = B0

gammaEpsilon :: [[Bit]] -> (Int, Int)
gammaEpsilon bits =
  both bin2dec (mc, lc)
  where
    mc = mostCommon bits
    lc = fmap flipBit mc

bin2dec :: [Bit] -> Int
bin2dec = cata go . reverse
  where
    go Nil = 0
    go (Cons x y) = f x + 2 * y

    f B0 = 0
    f B1 = 1

solvePart1 :: [[Bit]] -> IO Int
solvePart1 = pure . powerConsumption

lifeSupport :: [[Bit]] -> Int
lifeSupport = uncurry (*) . oxygenCO2

oxygenCO2 :: [[Bit]] -> (Int, Int)
oxygenCO2 bits = both bin2dec (oxygen bits, co2 bits)

oxygen :: [[Bit]] -> [Bit]
oxygen [x] = x
oxygen xs = case mostCommon xs of
  [] -> []
  (b : _) -> b : oxygen (fmap (drop 1) remaining)
    where
      remaining = filter ((b ==) . head) xs

co2 :: [[Bit]] -> [Bit]
co2 [x] = x
co2 xs = case fmap flipBit (mostCommon xs) of
  [] -> []
  (b : _) -> b : co2 (fmap (drop 1) remaining)
    where
      remaining = filter ((b ==) . head) xs

solvePart2 :: [[Bit]] -> IO Int
solvePart2 = pure . lifeSupport

sample :: T.Text
sample = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

parseBinary :: Parser [Bit]
parseBinary = some ((char '0' $> B0) <|> (char '1' $> B1))

parseReport :: Parser [[Bit]]
parseReport = P.delimited parseBinary space <* eof
