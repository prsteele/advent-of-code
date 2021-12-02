module Aoc.Y2021.P1Spec where

import Aoc.Y2021.P1
import qualified Data.Vector.Unboxed as V
import Test.Hspec

spec :: Spec
spec = do
  countIncreasesSpec
  windowSumSpec

sampleInput :: V.Vector Int
sampleInput =
  V.fromList
    [ 199,
      200,
      208,
      210,
      200,
      207,
      240,
      269,
      260,
      263
    ]

countIncreasesSpec :: Spec
countIncreasesSpec =
  describe "countIncreases" $ do
    it "computes sample input correctly" $
      countIncreases sampleInput `shouldBe` 7

windowSumSpec :: Spec
windowSumSpec =
  describe "windowSum" $ do
    it "computes sample input correctly" $
      windowSum 3 sampleInput `shouldBe` V.fromList [607, 618, 618, 617, 647, 716, 769, 792]
