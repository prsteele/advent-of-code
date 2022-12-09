{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2022.P7 where

import Aoc (Solution)
import Aoc.Parsers (Parser)
import qualified Aoc.Parsers as P
import Control.Monad
import Control.Monad.State
import Data.Functor (($>))
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

-- We store the path as a stack; the first element is the child
type Path = [T.Text]

data CdArg = Up | CdDir Path
  deriving (Show, Eq)

data Entry = Dir T.Text | Entry Int T.Text
  deriving (Show)

data Command = Cd CdArg | Ls [Entry]
  deriving (Show)

type ProblemInput = [Command]

parseCommand :: Parser Command
parseCommand = try parseCd <|> parseLs

parseFileName :: Parser T.Text
parseFileName = T.pack <$> some (letterChar <|> digitChar <|> char '.')

parseCd :: Parser Command
parseCd = chunk "$" *> hspace *> chunk "cd" *> hspace *> fmap Cd parseCdArg <* hspace <* eol

parseCdArg :: Parser CdArg
parseCdArg =
  chunk ".." $> Up
    <|> chunk "/" *> lookAhead eol $> CdDir []
    <|> CdDir <$> sepBy1 parseFileName (chunk "/")

parseLs :: Parser Command
parseLs = chunk "$" *> hspace *> chunk "ls" *> hspace *> eol *> fmap Ls parseEntries

parseEntries :: Parser [Entry]
parseEntries = P.linesOf (parseDir <|> parseEntry)
  where
    parseDir = Dir <$> (chunk "dir" *> hspace *> parseFileName)
    parseEntry = Entry <$> P.int <*> (hspace *> parseFileName)

parser :: Parser ProblemInput
parser = many parseCommand

solution :: Solution
solution input = do
  x <- P.parse parser input
  solvePart1 x >>= print
  solvePart2 x >>= print

modifyFst :: MonadState (a, b) m => (a -> a) -> m ()
modifyFst f = do
  (x, y) <- get
  put (f x, y)

modifySnd :: MonadState (a, b) m => (b -> b) -> m ()
modifySnd f = do
  (x, y) <- get
  put (x, f y)

processCommand :: MonadState (Path, M.Map Path [Entry]) m => Command -> m ()
processCommand (Cd Up) = modifyFst goUp
processCommand (Cd (CdDir to)) = modifyFst (to <>)
processCommand (Ls entries) = do
  currentDirectory <- gets fst
  modifySnd (M.union (M.singleton currentDirectory entries))

getListings :: ProblemInput -> M.Map Path [Entry]
getListings x = snd $ execState (mapM processCommand x) ([], M.empty)

shallowSizes :: M.Map Path [Entry] -> M.Map Path Int
shallowSizes = M.mapWithKey f
  where
    f k entries = sum (fmap (g k) entries)
    g _ (Entry entrySize _) = entrySize
    g _ _ = 0

getSizes :: M.Map Path [Entry] -> M.Map Path Int
getSizes = flip evalState M.empty . sizes'

sizes' :: MonadState (M.Map Path Int) m => M.Map Path [Entry] -> m (M.Map Path Int)
sizes' listings = sequence (M.mapWithKey (sizeOfDir listings) listings)

sizeOfDir :: MonadState (M.Map Path Int) m => M.Map Path [Entry] -> Path -> [Entry] -> m Int
sizeOfDir listings path entries = do
  memo <- get
  case M.lookup path memo of
    Just s -> pure s
    Nothing -> do
      s <- sum <$> mapM (sizeOfEntry listings path) entries
      modify (M.insert path s)
      pure s

sizeOfEntry :: MonadState (M.Map Path Int) m => M.Map Path [Entry] -> Path -> Entry -> m Int
sizeOfEntry _ _ (Entry entrySize _) = pure entrySize
sizeOfEntry listings parent (Dir name) = sizeOfDir listings (name : parent) (M.findWithDefault [] (name : parent) listings)

goUp :: Path -> Path
goUp [] = []
goUp (_ : xs) = xs

tshow :: Show a => a -> T.Text
tshow = T.pack . show

solvePart1 :: ProblemInput -> IO T.Text
solvePart1 input =
  let sizes = getSizes (getListings input)
      smallDirs = M.filter (<= 100000) sizes
   in pure . tshow $ sum (M.elems smallDirs)

solvePart2 :: ProblemInput -> IO T.Text
solvePart2 input =
  let totalSize = 70000000
      requiredSize = 30000000
      sizes = getSizes (getListings input)
      used = sizes M.! []
      available = totalSize - used
      toDelete = requiredSize - available
      ascending = sort (M.elems sizes)
   in pure . tshow . head . dropWhile (< toDelete) $ ascending

runOn :: FilePath -> IO ()
runOn = TIO.readFile >=> solution

p7 :: FilePath
p7 = "/home/prsteele/Documents/advent-of-code/data/2022/p7.txt"
