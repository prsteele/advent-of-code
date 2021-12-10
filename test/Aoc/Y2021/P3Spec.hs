{-# OPTIONS_GHC -fno-warn-orphans #-}

module Aoc.Y2021.P3Spec where

import qualified Aoc.Parsers as P
import Aoc.Y2021.P3
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..))

instance Arbitrary Bit where
  arbitrary = fmap f arbitrary
    where
      f :: Int -> Bit
      f n =
        if even n
          then B0
          else B1

spec :: Spec
spec = do
  part1Spec
  part2Spec

  describe "bitCounts" $ do
    it "counts bits properly (sample inputs)" $ do
      bitCounts [B0, B1, B0, B0, B1, B0] `shouldBe` (4, 2)
      bitCounts [] `shouldBe` (0, 0)

    prop "counts bits properly (arbitrary inputs)" $ do
      \bits -> bitCounts bits `shouldBe` naiveBitCounts bits

naiveBitCounts :: [Bit] -> (Int, Int)
naiveBitCounts bits = (count B0 bits, count B1 bits)
  where
    count b = length . filter (== b)

part1Spec :: Spec
part1Spec =
  describe "part 1" $ do
    it "computes sample input correctly" $ do
      (powerConsumption <$> P.parseText parser sample) `shouldBe` Right 198

part2Spec :: Spec
part2Spec =
  describe "part 2" $ do
    it "computes sample input correctly" $ do
      (lifeSupport <$> P.parseText parser sample) `shouldBe` Right 230
