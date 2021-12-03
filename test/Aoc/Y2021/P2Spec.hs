module Aoc.Y2021.P2Spec where

import qualified Data.Text as T
import Aoc.Y2021.P2
import Test.Hspec
import qualified Aoc.Parsers as P
import Text.Megaparsec

spec :: Spec
spec = do
  part1Spec
  part2Spec

part1Spec :: Spec
part1Spec =
  describe "part 1 correctness" $ do
    it "computes sample input correctly" $
      case P.parseText parser sampleInput of
        Left err -> print (errorBundlePretty err)
        Right input -> solvePart1 input `shouldBe` 150

part2Spec :: Spec
part2Spec =
  describe "part 1 correctness" $ do
    it "computes sample input correctly" $
      case P.parseText parser sampleInput of
        Left err -> print (errorBundlePretty err)
        Right input -> solvePart2 input `shouldBe` 900

sampleInput :: T.Text
sampleInput = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
