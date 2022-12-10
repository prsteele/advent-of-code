module Aoc where

import qualified Data.Text as T

type Solution = T.Text -> IO ()

tshow :: Show a => a -> T.Text
tshow = T.pack . show
