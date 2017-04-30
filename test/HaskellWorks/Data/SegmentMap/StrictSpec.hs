{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.SegmentMap.StrictSpec
  ( spec
  ) where

import Data.Foldable

import Control.Monad.IO.Class
import Data.Semigroup
import HaskellWorks.Data.FingerTree.Strict (ViewL (..), ViewR (..), viewl, viewr, (<|), (><), (|>))
import HaskellWorks.Data.SegmentMap.Strict

import qualified HaskellWorks.Data.FingerTree.Strict as FT
import qualified HaskellWorks.Data.SegmentMap.Strict as S (fromList)
import qualified Hedgehog.Gen                        as Gen
import qualified Hedgehog.Range                      as Range

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

fallbackTo :: Bool
fallbackTo = True

spec :: Spec
spec = describe "HaskellWorks.Data.SegmentMap.StrictSpec" $ do
    it "should convert SegmentMap to List" $ do
      let emptySM :: SegmentMap Int Int = empty
      segmentMapToList emptySM `shouldBe` []

    it "should convert List to SegmentMap" $ do
      let emptySM :: SegmentMap Int Int = empty
      let emptySM2 :: SegmentMap Int Int = S.fromList []
      segmentMapToList emptySM2 `shouldBe` segmentMapToList emptySM

    it "fromList with no overlap works" $ do
      let initial = fromList [(Segment 1 10, "1-10"), (Segment 11 20, "11-20")] :: SegmentMap Int String
      let expected = [(Segment 1 10, "1-10"), (Segment 11 20, "11-20")]
      segmentMapToList initial `shouldBe` expected

    it "insert with overlap works" $ do
      let initial = fromList [(Segment 1 10, "A"), (Segment 21 30, "C")] :: SegmentMap Int String
      let updated = insert (Segment 11 20) "B" initial
      let expected = [(Segment 1 10, "A"), (Segment 11 20, "B"), (Segment 21 30, "C")]
      segmentMapToList updated `shouldBe` expected

    it "insert with overlap works" $ do
      let initial = fromList [(Segment 1 10, "A"), (Segment 11 20, "C")] :: SegmentMap Int String
      let updated = insert (Segment 5 15) "B" initial
      let expected = [(Segment 1 4, "A"), (Segment 5 15, "B"), (Segment 16 20, "C")]
      segmentMapToList updated `shouldBe` expected

    it "fromList of two segments in order possibly overlapping" $ do
      require $ property $ do
        (Segment aLt aRt, Segment bLt bRt) <- forAll $ do
          aLt <- Gen.int (Range.linear 1   100)
          bRt <- Gen.int (Range.linear aLt 100)
          aRt <- Gen.int (Range.linear aLt bRt)
          bLt <- Gen.int (Range.linear aLt bRt)
          return (Segment aLt aRt, Segment bLt bRt)
        let initial = [(Segment aLt aRt, "A"), (Segment bLt bRt, "B")] :: [(Segment Int, String)]
        let actual = segmentMapToList (fromList initial)
        let aRt' = aRt `min` pred bLt
        case () of
          () | aLt == bRt               -> actual === [(Segment aLt bRt , "B")]
          () | bLt <= aLt && bRt >= aRt -> actual === [(Segment bLt bRt , "B")]
          () | aRt >= bLt               -> actual === [(Segment aLt aRt', "A"), (Segment bLt bRt, "B")]
          () | fallbackTo               -> actual === [(Segment aLt aRt , "A"), (Segment bLt bRt, "B")]

    it "fromList [(Segment 1 1, \"A\"), (Segment 1 1, \"B\")]" $ do
      let initial = [(Segment 1 1, "A"), (Segment 1 1, "B")] :: [(Segment Int, String)]
      let actual = segmentMapToList (fromList initial)
      actual `shouldBe` [(Segment 1 1, "B")]

    it "fromList [(Segment 1 2, \"A\"), (Segment 1 1, \"B\")]" $ do
      let initial = [(Segment 1 2, "A"), (Segment 1 1, "B")] :: [(Segment Int, String)]
      let actual = segmentMapToList (fromList initial)
      actual `shouldBe` [(Segment 1 1, "B"), (Segment 2 2, "A")]

    it "fromList [(Segment 1 2, \"A\"), (Segment 2 2, \"B\")]" $ do
      let initial = [(Segment 1 2, "A"), (Segment 2 2, "B")] :: [(Segment Int, String)]
      let actual = segmentMapToList (fromList initial)
      actual `shouldBe` [(Segment 1 1, "A"), (Segment 2 2, "B")]

    it "fromList [(Segment 1 2, \"A\"), (Segment 1 2, \"B\")]" $ do
      let initial = [(Segment 1 2, "A"), (Segment 1 2, "B")] :: [(Segment Int, String)]
      let actual = segmentMapToList (fromList initial)
      actual `shouldBe` [(Segment 1 2, "B")]

    it "fromList [(Segment 1 3, \"A\"), (Segment 1 1, \"B\")]" $ do
      let initial = [(Segment 1 3, "A"), (Segment 1 1, "B")] :: [(Segment Int, String)]
      let actual = segmentMapToList (fromList initial)
      actual `shouldBe` [(Segment 1 1, "B"), (Segment 2 3, "A")]

    it "fromList [(Segment 1 3, \"A\"), (Segment 3 3, \"B\")]" $ do
      let initial = [(Segment 1 3, "A"), (Segment 3 3, "B")] :: [(Segment Int, String)]
      let actual = segmentMapToList (fromList initial)
      actual `shouldBe` [(Segment 1 2, "A"), (Segment 3 3, "B")]

    it "fromList [(Segment 1 3, \"A\"), (Segment 2 2, \"B\")]" $ do
      let initial = [(Segment 1 3, "A"), (Segment 2 2, "B")] :: [(Segment Int, String)]
      let actual = segmentMapToList (fromList initial)
      actual `shouldBe` [(Segment 1 1, "A"), (Segment 2 2, "B"), (Segment 3 3, "A")]

    it "fromList [(Segment 1 3, \"A\"), (Segment 0 1, \"B\")]" $ do
      let initial = [(Segment 1 3, "A"), (Segment 0 1, "B")] :: [(Segment Int, String)]
      let actual = segmentMapToList (fromList initial)
      actual `shouldBe` [(Segment 0 1, "B"), (Segment 2 3, "A")]

    it "fromList [(Segment 1 3, \"A\"), (Segment 3 4 \"B\")]" $ do
      let initial = [(Segment 1 3, "A"), (Segment 3 4, "B")] :: [(Segment Int, String)]
      let actual = segmentMapToList (fromList initial)
      actual `shouldBe` [(Segment 1 2, "A"), (Segment 3 4, "B")]

    it "fromList [(Segment 1 2, \"A\"), (Segment 2 7, \"B\")]" $ do
      let initial = [(Segment 1 2, "A"), (Segment 2 7, "B")] :: [(Segment Int, String)]
      let actual = segmentMapToList (fromList initial)
      actual `shouldBe` [(Segment 1 1, "A"), (Segment 2 7, "B")]

    describe "cappedL" $ do
      let original = FT.Single (Item (Max (11  :: Int)) (Segment {low = 11 :: Int, high = 20}, "A" :: String))
      it "left of" $ do
        cappedL  5 original `shouldBe` (FT.Empty, FT.Empty)
      it "overlapping" $ do
        cappedL 15 original `shouldBe` (FT.Single (Item (Max (11  :: Int)) (Segment {low = 11 :: Int, high = 14}, "A" :: String)), FT.Single (Item (Max 15) (Segment {low = 15, high = 20}, "A")))
      it "right of" $ do
        cappedL 25 original `shouldBe` (FT.Single (Item (Max (11  :: Int)) (Segment {low = 11 :: Int, high = 20}, "A" :: String)), FT.Empty)
    describe "cappedM" $ do
      let original = FT.Single (Item (Max (21 :: Int)) (Segment {low = 21 :: Int, high = 30}, "C" :: String))
      it "left of" $ do
        cappedM 15 original `shouldBe` FT.Single (Item (Max (21 :: Int)) (Segment {low = 21 :: Int, high = 30}, "C" :: String))
      it "overlapping" $ do
        cappedM 25 original `shouldBe` FT.Single (Item (Max (26 :: Int)) (Segment {low = 26 :: Int, high = 30}, "C" :: String))
      it "left of" $ do
        cappedM 35 original `shouldBe` FT.Empty

    it "should have require function that checks hedgehog properties" $ do
      require $ property $ do
        x <- forAll (Gen.int Range.constantBounded)
        x === x
