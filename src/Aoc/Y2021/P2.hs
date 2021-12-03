{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2021.P2 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

data Direction
  = Forward Int
  | Up Int
  | Down Int
  deriving (Eq, Show)

data Position = Position
  { _positionHorizontal :: Int
  , _positionVertical :: Int
  }
  deriving (Eq, Show)

instance Semigroup Position where
  Position x y <> Position x' y' = Position (x + x') (y + y')

instance Monoid Position where
  mempty = Position 0 0

data Attitude = Attitude
  { _attitudeAim :: Int
  , _attitudePosition :: Position
  }
  deriving (Eq, Show)

parseDirection :: Parser Direction
parseDirection =
  (Forward <$> (string "forward " *> P.int))
    <|> (Up <$> (string "up " *> P.int))
    <|> (Down <$> (string "down " *> P.int))

parser :: Parser (V.Vector Direction)
parser = V.fromList <$> P.delimited parseDirection space <* eof

solution :: Solution
solution input = do
  v <- P.parse parser input
  print (solvePart1 v)
  print (solvePart2 v)

solvePart1 :: V.Vector Direction -> Int
solvePart1 v = horizontal * vertical
  where
    Position horizontal vertical = follow v

solvePart2 :: V.Vector Direction -> Int
solvePart2 v = horizontal * vertical
  where
    Attitude _ (Position horizontal vertical) = followAim v

follow :: Foldable t => t Direction -> Position
follow = foldr (mappend . go) (Position 0 0)
  where
    go (Up x) = Position 0 (-x)
    go (Down x) = Position 0 x
    go (Forward x) = Position x 0

followAim :: Foldable t => t Direction -> Attitude
followAim = foldl (flip adjust) (Attitude 0 mempty)

adjust :: Direction -> Attitude -> Attitude
adjust (Down x) a@(Attitude aim _) = a { _attitudeAim = aim + x }
adjust (Up x) a@(Attitude aim _) = a { _attitudeAim = aim - x }
adjust (Forward x) a@(Attitude aim (Position h v)) = a { _attitudePosition = Position (h + x) (v + aim * x)}
