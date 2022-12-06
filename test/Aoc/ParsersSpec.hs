{-# LANGUAGE OverloadedStrings #-}

module Aoc.ParsersSpec where

import Aoc.Parsers
import Control.Monad (forM_)
import Data.Char (ord)
import qualified Data.Map.Strict as M
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Char

spec :: Spec
spec = do
  indexedSpec

indexedSpec :: Spec
indexedSpec =
  describe "indexed" $ do
    it "can parse grids" $
      let sample = "123\n456\n789\n"
          parser = fmap (\c -> ord c - ord '0') numberChar
          result = parseText (indexed parser) sample
       in case result of
            Left f -> expectationFailure (errorBundlePretty f)
            Right grid ->
              forM_ [0 .. 2] $ \row ->
                forM_ [0 .. 2] $ \col ->
                  grid M.!? (row, col) `shouldBe` Just (row * 3 + col + 1)
    it "can parse irregular grids" $
      let sample = "12\n456\n7\n"
          parser = fmap (\c -> ord c - ord '0') numberChar
          result = parseText (indexed parser) sample
       in case result of
            Left f -> expectationFailure (errorBundlePretty f)
            Right grid ->
              grid
                `shouldBe` ( M.fromList
                               [ ((0, 0), 1),
                                 ((0, 1), 2),
                                 ((1, 0), 4),
                                 ((1, 1), 5),
                                 ((1, 2), 6),
                                 ((2, 0), 7)
                               ]
                           )
