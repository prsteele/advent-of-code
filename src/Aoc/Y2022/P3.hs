{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P3 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Data.Char (ord)
import qualified Data.Set as S
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

type ProblemInput = V.Vector Rucksack

newtype Item = Item Char
  deriving (Show, Ord, Eq)

type Rucksack = (V.Vector Item, V.Vector Item)

priority :: Item -> Int
priority (Item c) =
  let isLower = ord 'a' <= ord c && ord c <= ord 'z'
   in if isLower
        then ord c - ord 'a' + 1
        else ord c - ord 'A' + 27

parseItem :: Parser Item
parseItem = Item <$> alphaNumChar

parseRucksack :: Parser Rucksack
parseRucksack = do
  items <- V.fromList <$> many parseItem
  let n = V.length items
      half = n `div` 2
  pure (V.slice 0 half items, V.slice half half items)

parser :: Parser ProblemInput
parser = V.fromList <$> P.linesOf parseRucksack <* eof

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

vector2Set :: Ord a => V.Vector a -> S.Set a
vector2Set = S.fromList . V.toList

mispacked :: Rucksack -> S.Set Item
mispacked (left, right) =
  let leftSet = vector2Set left
      rightSet = vector2Set right
   in S.intersection leftSet rightSet

solvePart1 :: ProblemInput -> IO Int
solvePart1 = pure . V.sum . fmap (sum . fmap priority . S.toList . mispacked)

groups :: Int -> V.Vector a -> V.Vector (V.Vector a)
groups n xs = V.fromList [V.slice i (len i) xs | i <- [0, n .. V.length xs - 1]]
  where
    len i = min n (V.length xs - i)

unique :: V.Vector (S.Set Item) -> S.Set Item
unique xs = case xs V.!? 0 of
  Nothing -> S.empty
  Just x -> V.foldr S.intersection x (V.slice 1 (V.length xs - 1) xs)

rucksackSet :: Rucksack -> S.Set Item
rucksackSet (x, y) = S.union (vector2Set x) (vector2Set y)

solvePart2 :: ProblemInput -> IO Int
solvePart2 =
  pure . sum . fmap (badgesPriority . groupBadges) . groups 3
  where
    groupBadges = unique . fmap rucksackSet
    badgesPriority = sum . fmap priority . S.toList
