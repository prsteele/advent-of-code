module Aoc.Y2020.P1Spec where

import Aoc.Y2020.P1
import Data.Maybe (isNothing)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = do
  twoSumSpec
  threeSumSpec

twoSumSpec :: Spec
twoSumSpec =
  describe "twoSum" $ do
    it "computes sample input correctly" $ do
      answer <- twoSum (V.fromList [1721, 979, 366, 299, 675, 1456]) 2020
      answer `shouldBe` Just (299, 1721)

threeSumSpec :: Spec
threeSumSpec =
  describe "threeSum" $ do
    it "computes sample input correctly" $ do
      answer <- threeSum (V.fromList [1721, 979, 366, 299, 675, 1456]) 2020
      answer `shouldBe` Just (366, 675, 979)
    prop "computes correct results on arbitrary inputs" $
      \nums -> monadicIO $ do
        let v = V.fromList nums
        if V.length v < 3
          then do
            -- Trivial case where there aren't three elements
            target <- pick arbitrary
            result <- run (threeSum v target)
            assert (isNothing result)
          else -- Regular case where there are at least three elements

            let target = V.sum (V.slice 0 3 v)
             in do
                  result <- run (threeSum v target)
                  case result of
                    Nothing -> assert False
                    Just (x, y, z) -> assert (x + y + z == target)
    prop "computes correct results on arbitrary inputs (where there is no match)" $
      \nums -> monadicIO $ do
        let v = V.fromList nums
        let target = sum (filter (> 0) (V.toList v)) + 1
        result <- run (threeSum v target)
        assert (isNothing result)
