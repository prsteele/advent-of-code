module Main where

import Aoc.Problems
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Clock
import System.Exit
import Text.Printf

timed :: IO () -> IO TimeSpec
timed runner = do
  t0 <- getTime MonotonicRaw
  runner
  t1 <- getTime MonotonicRaw
  pure (t1 - t0)

data InputSource
  = File FilePath
  | StdIn
  deriving (Show)

data Opts = Opts
  { _problem :: String,
    _year :: String,
    _input :: InputSource
  }
  deriving (Show)

parseFile :: Parser InputSource
parseFile =
  File
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "the file to process"
      )

parseStdIn :: Parser InputSource
parseStdIn =
  flag'
    StdIn
    ( long "stdin"
        <> help "read from stdin"
    )

parseOpts :: Parser Opts
parseOpts =
  Opts
    <$> strOption
      ( long "problem"
          <> short 'p'
          <> help "the problem number to run"
      )
    <*> strOption
      ( long "year"
      <> short 'y'
      <> help "the competition year to run"
      <> value "2021")
    <*> (parseFile <|> parseStdIn)

run :: Opts -> IO ()
run (Opts problem year input) =
  case M.lookup year problems >>= M.lookup problem of
    Nothing -> putStrLn ("Unknown problem " <> problem) >> exitFailure
    Just p -> do
      inputText <- case input of
        File fname -> TIO.readFile fname
        StdIn -> TIO.getContents
      ts <- timed (p inputText)
      let t = fromIntegral (toNanoSecs ts) / 1e9 :: Double
      putStrLn $ printf "(ran in %.3fs)" t

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (parseOpts <**> helper)
        ( fullDesc
            <> progDesc "Advent of Code solutions"
        )
