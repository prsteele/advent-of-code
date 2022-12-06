{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P5 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Control.Monad
import Control.Monad.State
import Data.Functor (($>))
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
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

type CargoState = Stacks

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

move :: MonadState CargoState m => Int -> Int -> m ()
move from to = do
  stacks <- get
  case (stacks M.!? from, stacks M.!? to) of
    (Just (s : ss), Just dest) -> do
      modify (M.insert from ss)
      modify (M.insert to (s : dest))
    _ -> pure ()

followOrder :: MonadState CargoState m => Order -> m ()
followOrder order = replicateM_ (orderCount order) (move (orderSource order) (orderDest order))

followOrder' :: MonadState CargoState m => Order -> m ()
followOrder' order = do
  stacks <- get
  let from = orderSource order
      to = orderDest order
      cnt = orderCount order
  case (stacks M.!? from, stacks M.!? to) of
    (Just source, Just dest) -> do
      modify (M.insert from (drop cnt source))
      modify (M.insert to (take cnt source ++ dest))
    _ -> pure ()

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

topCrates :: Stacks -> [Crate]
topCrates stacks =
  let cols = sort (M.keys stacks)
   in catMaybes [safeHead (stacks M.! c) | c <- cols]

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

solvePart1 :: ProblemInput -> IO T.Text
solvePart1 (stacks, orders) =
  pure . T.concat . topCrates . flip execState stacks $ mapM_ followOrder orders

solvePart2 :: ProblemInput -> IO T.Text
solvePart2 (stacks, orders) =
  pure . T.concat . topCrates . flip execState stacks $ mapM_ followOrder' orders
