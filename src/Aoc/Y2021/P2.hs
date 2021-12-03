{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2021.P2 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

solution :: Solution
solution input = do
  v <- P.parse parser input
  print (solvePart1 v)
  print (solvePart2 v)

data Direction
  = Forward Int
  | Up Int
  | Down Int
  deriving (Eq, Show)

data Position = Position Int Int
  deriving (Eq, Show)

data Attitude = Attitude Int Position
  deriving (Eq, Show)

parseDirection :: Parser Direction
parseDirection =
  (Forward <$> (string "forward " *> P.int))
    <|> (Up <$> (string "up " *> P.int))
    <|> (Down <$> (string "down " *> P.int))

solvePart1 :: V.Vector Direction -> Int
solvePart1 v = horizontal * vertical
  where
    Position horizontal vertical = part1FollowDirections v

part1Follow :: Position -> Direction -> Position
part1Follow (Position h v) (Up x) = Position h (v - x)
part1Follow (Position h v) (Down x) = Position h (v + x)
part1Follow (Position h v) (Forward x) = Position (h + x) v

part1FollowDirections :: Foldable t => t Direction -> Position
part1FollowDirections = foldl part1Follow (Position 0 0)

parser :: Parser (V.Vector Direction)
parser = V.fromList <$> P.delimited parseDirection space <* eof

solvePart2 :: V.Vector Direction -> Int
solvePart2 v = horizontal * vertical
  where
    (Position horizontal vertical) = part2FollowDirections v

part2FollowDirections :: Foldable t => t Direction -> Position
part2FollowDirections directions = position
  where
    Attitude _ position = foldl part2Follow (Attitude 0 (Position 0 0)) directions

part2Follow :: Attitude -> Direction -> Attitude
part2Follow (Attitude aim p) (Down x) = Attitude (aim + x) p
part2Follow (Attitude aim p) (Up x) = Attitude (aim - x) p
part2Follow (Attitude aim (Position h v)) (Forward x) = Attitude aim (Position (h + x) (v + aim * x))
