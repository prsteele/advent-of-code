{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P11 where

import Aoc (Solution, tshow)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Control.Monad
import Control.Monad.State
import Data.Functor (($>))
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

type ProblemInput = [Monkey]

data Ref = RefInt Int | RefOld
  deriving (Show, Read)

data Op
  = Mult Ref
  | Add Ref
  deriving (Show, Read)

newtype Test = Divisible Int
  deriving (Show, Read)

newtype MonkeyId = MonkeyId Int
  deriving (Show, Read, Ord, Eq)

data Monkey = Monkey
  { _id :: MonkeyId,
    _items :: [Int],
    _op :: Op,
    _test :: Test,
    _trueTarget :: MonkeyId,
    _falseTarget :: MonkeyId,
    _inspections :: Int
  }
  deriving (Show, Read)

parseMonkey :: Parser Monkey
parseMonkey = do
  mid <- chunk "Monkey " *> P.int <* chunk ":" <* eol
  items <- hspace *> chunk "Starting items: " *> sepBy P.int (chunk ", ") <* eol
  op <- hspace *> chunk "Operation: " *> parseOp <* hspace <* eol
  test <- hspace *> chunk "Test: " *> parseTestExpr <* eol
  trueTarget <- hspace *> chunk "If true: throw to monkey " *> P.int <* eol
  falseTarget <- hspace *> chunk "If false: throw to monkey " *> P.int <* eol

  pure
    ( Monkey
        { _id = MonkeyId mid,
          _items = items,
          _op = op,
          _test = test,
          _trueTarget = MonkeyId trueTarget,
          _falseTarget = MonkeyId falseTarget,
          _inspections = 0
        }
    )

parseOp :: Parser Op
parseOp =
  Mult <$> (chunk "new = old *" *> hspace *> parseRef)
    <|> Add <$> (chunk "new = old +" *> hspace *> parseRef)

parseRef :: Parser Ref
parseRef =
  chunk "old" $> RefOld
    <|> (RefInt <$> P.int)

parseTestExpr :: Parser Test
parseTestExpr = Divisible <$> (chunk "divisible by " *> P.int)

parser :: Parser ProblemInput
parser = sepBy parseMonkey space

type WorryState = M.Map MonkeyId Monkey

resolveRef :: Ref -> Int -> Int
resolveRef RefOld old = old
resolveRef (RefInt x) _ = x

applyOp :: Op -> Int -> Int
applyOp (Mult ref) worry = worry * resolveRef ref worry
applyOp (Add ref) worry = worry + resolveRef ref worry

inspect :: MonadState WorryState m => (Int -> Int) -> MonkeyId -> m ()
inspect worryUpdate mid = do
  monkies <- get

  let monkey = monkies M.! mid
      items = _items monkey

  case items of
    [] -> pure ()
    (item : rest) -> do
      let op = _op monkey
          item' = worryUpdate (applyOp op item)
          monkey' = monkey {_inspections = _inspections monkey + 1, _items = item' : rest}
          monkies' = M.insert mid monkey' monkies

      put monkies'

evalTest :: Test -> Int -> Bool
evalTest (Divisible i) worry = worry `mod` i == 0

applyTest :: MonadState WorryState m => MonkeyId -> m ()
applyTest mid = do
  monkies <- get

  let monkey = monkies M.! mid
      items = _items monkey

  case items of
    [] -> pure ()
    (item : rest) -> do
      let outcome = evalTest (_test monkey) item
          target =
            if outcome
              then _trueTarget monkey
              else _falseTarget monkey

          -- Throw the item
          targetMonkey = monkies M.! target
          targetMonkey' = targetMonkey {_items = _items targetMonkey ++ [item]}
          monkey' = monkey {_items = rest}

      put (M.insert target targetMonkey' (M.insert mid monkey' monkies))

applyTurn :: MonadState WorryState m => (Int -> Int) -> MonkeyId -> m ()
applyTurn worryUpdate mid = do
  monkies <- get
  let monkey = monkies M.! mid
  forM_ (_items monkey) (const (inspect worryUpdate mid >> applyTest mid))

applyRound :: MonadState WorryState m => (Int -> Int) -> m ()
applyRound worryUpdate = do
  monkies <- get
  let mids = sort (M.keys monkies)
  forM_ mids (applyTurn worryUpdate)

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

part1WorryUpdate :: Integral a => a -> a
part1WorryUpdate = (`div` 3)

part2WorryUpdate :: Integral a => a -> (a -> a)
part2WorryUpdate modulus = (`mod` modulus)

simulate :: Int -> (Int -> Int) -> ProblemInput -> Int
simulate rounds worryUpdate monkies =
  let monkeyMap = M.fromList (zip (fmap _id monkies) monkies)
      result = execState (replicateM rounds (applyRound worryUpdate)) monkeyMap
   in monkeyBusiness result

monkeyBusiness :: M.Map MonkeyId Monkey -> Int
monkeyBusiness monkeyMap =
  let sorted = sortOn (Down . _inspections) (M.elems monkeyMap)
      top2 = fmap _inspections (take 2 sorted)
   in product top2

solvePart1 :: ProblemInput -> IO T.Text
solvePart1 = pure . tshow . simulate 20 part1WorryUpdate

solvePart2 :: ProblemInput -> IO T.Text
solvePart2 monkies =
  let tests = fmap _test monkies
      mods = [i | Divisible i <- tests]
      modulus = foldr lcm 1 mods
   in pure . tshow . simulate 10000 (part2WorryUpdate modulus) $ monkies

runOn :: FilePath -> IO ()
runOn = TIO.readFile >=> solution

problem :: FilePath
problem = "data/2022/p11.txt"
