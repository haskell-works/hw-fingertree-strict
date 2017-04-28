{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.SegmentMap.FingerTree.StrictSpec
  ( spec
  ) where

import Data.Foldable

import Data.Semigroup
import HaskellWorks.Data.SegmentMap.FingerTree.Strict
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import HaskellWorks.Data.FingerTree.Strict ((<|), (><))

import qualified HaskellWorks.Data.FingerTree.Strict            as FT
import qualified HaskellWorks.Data.SegmentMap.FingerTree.Strict as S (fromList)
import qualified Hedgehog.Gen                                   as Gen
import qualified Hedgehog.Range                                 as Range

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.SegmentMap.StrictSpec" $ do
    it "should convert SegmentMap to List" $ do
      let emptySM :: SegmentMap Int Int = empty
      segmentMapToList emptySM `shouldBe` []

    it "should convert List to SegmentMap" $ do
      let emptySM :: SegmentMap Int Int = empty
      let emptySM2 :: SegmentMap Int Int = S.fromList []
      segmentMapToList emptySM2 `shouldBe` segmentMapToList emptySM

    it "simpler unit test" $ do
      let initial = fromList [(Segment 1 10, "1-10"), (Segment 11 20, "11-20")] :: SegmentMap Int String
      let expected = [(Segment 1 10, "1-10"), (Segment 11 20, "11-20")]
      segmentMapToList initial `shouldBe` expected

    it "simple unit test" $ do
      let initial = fromList [(Segment 1 10, "1-10"), (Segment 11 20, "11-20")] :: SegmentMap Int String
      let updated = insert (Segment 5 15) "5-15" initial
      let expected = [(Segment 1 4, "1-10"), (Segment 5 15, "5-15"), (Segment 16 20, "10-20")]
      print initial
      let     s = Segment 5 15
              Segment lo hi = Segment 5 15
              Just x = Just "5-15"
              SegmentMap (OrderedMap t) = initial
              (lt, ys) = FT.split (>= Min lo) t
              (_, rt)  = FT.split (> Min hi) ys
              resl = cappedL lo lt
              resm = Node (Min lo) (s, x)
              resr = cappedR hi rt
              result = SegmentMap $ OrderedMap (resl >< resm <| resr)
              in
          print result
      print expected
      segmentMapToList updated `shouldBe` expected

    it "should have require function that checks hedgehog properties" $ do
      require $ property $ do
        x <- forAll (Gen.int Range.constantBounded)
        x === x
