{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P5 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Data.Functor (($>))
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

type Crate = T.Text

data Order = Order
  { orderCount :: Int,
    orderSource :: Int,
    orderDest :: Int
  }

instance Show Order where
  show order =
    "move "
      <> show (orderCount order)
      <> " from "
      <> show (orderSource order)
      <> " to "
      <> show (orderDest order)

type Stack = [Crate]

type Stacks = M.Map Int Stack

type ProblemInput = (Stacks, [Order])

parseCrate :: Parser T.Text
parseCrate = chunk "[" *> fmap (T.pack . pure) letterChar <* chunk "]"

parseGridEntry :: Parser (Maybe Crate)
parseGridEntry = fmap Just parseCrate <|> chunk "   " $> Nothing

parseGrid :: Parser (M.Map (Int, Int) Crate)
parseGrid = M.mapMaybe id <$> P.indexed (parseGridEntry <* (chunk " " <|> mempty))

parseOrder :: Parser Order
parseOrder = do
  cnt <- chunk "move" *> hspace *> P.int
  source <- hspace *> chunk "from" *> hspace *> P.int
  Order cnt source <$> (hspace *> chunk "to" *> hspace *> P.int <* hspace)

parser :: Parser ProblemInput
parser = do
  crates <- fmap createStacks parseGrid
  _ <- hspace <* sepEndBy P.int hspace <* eol
  _ <- eol
  orders <- P.linesOf parseOrder
  pure (crates, orders)

createStacks :: M.Map (Int, Int) Crate -> M.Map Int Stack
createStacks grid =
  let cols = snd <$> M.keys grid
      rows = fst <$> M.keys grid
      rowRange = [minimum rows .. maximum rows]
      mkStack col = catMaybes [grid M.!? (row, col) | row <- rowRange]
   in M.fromList [(col + 1, mkStack col) | col <- cols]

pop :: Stack -> (Maybe Crate, Stack)
pop [] = (Nothing, [])
pop (x : xs) = (Just x, xs)

push :: Crate -> Stack -> Stack
push x xs = x : xs

shift :: Int -> Stack -> Stack -> (Stack, Stack)
shift 0 x y = (x, y)
shift n x y = shift (n - 1) x' y'
  where
    (z, x') = pop x
    y' = case z of
      Just z' -> push z' y
      Nothing -> y

followOrder :: Stacks -> Order -> Stacks
followOrder stacks order =
  let cnt = orderCount order
      source = orderSource order
      dest = orderDest order
   in case (stacks M.!? source, stacks M.!? dest) of
        (Just sourceStack, Just destStack) -> M.insert dest destStack' (M.insert source sourceStack' stacks)
          where
            (sourceStack', destStack') = shift cnt sourceStack destStack
        _ -> stacks

followOrder' :: Stacks -> Order -> Stacks
followOrder' stacks order =
  let cnt = orderCount order
      source = orderSource order
      dest = orderDest order
   in case (stacks M.!? source, stacks M.!? dest) of
        (Just sourceStack, Just destStack) -> M.insert dest destStack' (M.insert source sourceStack' stacks)
          where
            sourceStack' = drop cnt sourceStack
            destStack' = take cnt sourceStack ++ destStack
        _ -> stacks

followOrders :: Stacks -> [Order] -> Stacks
followOrders = foldl followOrder

followOrders' :: Stacks -> [Order] -> Stacks
followOrders' = foldl followOrder'

topCrates :: Stacks -> [Crate]
topCrates stacks =
  let cols = sort (M.keys stacks)
   in catMaybes [fst (pop (stacks M.! c)) | c <- cols]

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

solvePart1 :: ProblemInput -> IO T.Text
solvePart1 (stacks, orders) = pure . T.concat . topCrates . followOrders stacks $ orders

solvePart2 :: ProblemInput -> IO T.Text
solvePart2 (stacks, orders) = pure . T.concat . topCrates . followOrders' stacks $ orders
