{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.SegmentSet.FingerTree.StrictSpec
  ( spec
  ) where

import Data.Foldable

import Control.Monad.IO.Class
import Data.Semigroup
import HaskellWorks.Data.SegmentSet.FingerTree.Strict
import HaskellWorks.Hspec.Hedgehog
import Hedgehog

import HaskellWorks.Data.FingerTree.Strict (ViewL (..), ViewR (..), viewl, viewr, (<|), (><), (|>))

import qualified HaskellWorks.Data.FingerTree.Strict            as FT
import qualified HaskellWorks.Data.SegmentSet.FingerTree.Strict as S (fromList)
import qualified Hedgehog.Gen                                   as Gen
import qualified Hedgehog.Range                                 as Range

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

fallbackTo :: Bool
fallbackTo = True

spec :: Spec
spec = describe "HaskellWorks.Data.SegmentSet.StrictSpec" $ do
    it "should convert SegmentSet to List" $ do
      let emptySM :: SegmentSet Int = empty
      segmentSetToList emptySM `shouldBe` []

    it "should convert List to SegmentSet" $ do
      let emptySM :: SegmentSet Int = empty
      let emptySM2 :: SegmentSet Int = S.fromList []
      segmentSetToList emptySM2 `shouldBe` segmentSetToList emptySM

    it "fromList with no overlap works" $ do
      let initial = fromList [Segment 1 10, Segment 11 20] :: SegmentSet Int
      let expected = [Segment 1 20]
      segmentSetToList initial `shouldBe` expected

    it "insert with overlap works" $ do
      let initial = fromList [Segment 1 10, Segment 21 30] :: SegmentSet Int
      let updated = insert (Segment 11 20) initial
      let expected = [Segment 1 30]
      segmentSetToList updated `shouldBe` expected

    it "insert with overlap works" $ do
      let initial = fromList [Segment 1 10, Segment 11 20] :: SegmentSet Int
      let updated = insert (Segment 5 15) initial
      let expected = [Segment 1 20]
      segmentSetToList updated `shouldBe` expected

    it "fromList of two segments in order possibly overlapping" $ do
      require $ property $ do
        (Segment aLt aRt, Segment bLt bRt) <- forAll $ do
          aLt <- Gen.int (Range.linear 1   100)
          bRt <- Gen.int (Range.linear aLt 100)
          aRt <- Gen.int (Range.linear aLt bRt)
          bLt <- Gen.int (Range.linear aLt bRt)
          return (Segment aLt aRt, Segment bLt bRt)
        let initial = [Segment aLt aRt, Segment bLt bRt] :: [Segment Int]
        let actual = segmentSetToList (fromList initial)
        let aRt' = aRt `min` pred bLt
        case () of
          () | aLt == bRt               -> actual === [Segment aLt bRt]
          () | bLt <= aLt && bRt >= aRt -> actual === [Segment bLt bRt]
          () | succ aRt >= bLt          -> actual === [Segment aLt bRt]
          () | fallbackTo               -> actual === [Segment aLt aRt , Segment bLt bRt]

    it "fromList [Segment 1 1, Segment 1 1]" $ do
      let initial = [Segment 1 1, Segment 1 1] :: [Segment Int]
      let actual = segmentSetToList (fromList initial)
      actual `shouldBe` [Segment 1 1]

    it "fromList [Segment 1 2, Segment 1 1]" $ do
      let initial = [Segment 1 2, Segment 1 1] :: [Segment Int]
      let actual = segmentSetToList (fromList initial)
      actual `shouldBe` [Segment 1 2]

    it "fromList [Segment 1 2, Segment 2 2]" $ do
      let initial = [Segment 1 2, Segment 2 2] :: [Segment Int]
      let actual = segmentSetToList (fromList initial)
      actual `shouldBe` [Segment 1 2]

    it "fromList [Segment 1 2, Segment 1 2]" $ do
      let initial = [Segment 1 2, Segment 1 2] :: [Segment Int]
      let actual = segmentSetToList (fromList initial)
      actual `shouldBe` [Segment 1 2]

    it "fromList [Segment 1 3, Segment 1 1]" $ do
      let initial = [Segment 1 3, Segment 1 1] :: [Segment Int]
      let actual = segmentSetToList (fromList initial)
      actual `shouldBe` [Segment 1 3]

    it "fromList [Segment 1 3, Segment 3 3]" $ do
      let initial = [Segment 1 3, Segment 3 3] :: [Segment Int]
      let actual = segmentSetToList (fromList initial)
      actual `shouldBe` [Segment 1 3]

    it "fromList [Segment 1 3, Segment 2 2]" $ do
      let initial = [Segment 1 3, Segment 2 2] :: [Segment Int]
      let actual = segmentSetToList (fromList initial)
      actual `shouldBe` [Segment 1 3]

    it "fromList [Segment 1 3, Segment 0 1]" $ do
      let initial = [Segment 1 3, Segment 0 1] :: [Segment Int]
      let actual = segmentSetToList (fromList initial)
      actual `shouldBe` [Segment 0 3]

    it "fromList [Segment 1 3, Segment 3 4]" $ do
      let initial = [Segment 1 4] :: [Segment Int]
      let actual = segmentSetToList (fromList initial)
      actual `shouldBe` [Segment 1 4]

    it "fromList [Segment 1 2, Segment 2 7]" $ do
      let initial = [Segment 1 7] :: [Segment Int]
      let actual = segmentSetToList (fromList initial)
      actual `shouldBe` [Segment 1  7]

    it "fromList (delete (Segment 1 1) [Segment 1 1])" $ do
      let initial = [Segment 1 1] :: [Segment Int]
      let actual = segmentSetToList (delete (Segment 1 1) (fromList initial))
      actual `shouldBe` []

    it "fromList (delete (Segment 1 3) [Segment 2 4])" $ do
      let initial = [Segment 2 4] :: [Segment Int]
      let actual = segmentSetToList (delete (Segment 1 3) (fromList initial))
      actual `shouldBe` [Segment 4 4]

    it "fromList (delete (Segment 3 5) [Segment 2 4])" $ do
      let initial = [Segment 2 4] :: [Segment Int]
      let actual = segmentSetToList (delete (Segment 3 5) (fromList initial))
      actual `shouldBe` [Segment 2 2]

    it "fromList (delete (Segment 3 5) [Segment 2 4])" $ do
      let initial = [Segment 2 4] :: [Segment Int]
      let actual = segmentSetToList (delete (Segment 3 3) (fromList initial))
      actual `shouldBe` [Segment 2 2, Segment 4 4]

    describe "cappedL" $ do
      let original = FT.Single (Item (Max (11  :: Int)) (Segment 11 20))
      it "left of" $ do
        cappedL  5 original `shouldBe` (FT.Empty, FT.Empty)
      it "overlapping" $ do
        cappedL 15 original `shouldBe` (FT.Single (Item (Max (11  :: Int)) (Segment 11 14)), FT.Single (Item (Max 15) (Segment 15 20)))
      it "right of" $ do
        cappedL 25 original `shouldBe` (FT.Single (Item (Max 11) (Segment 11 20)), FT.Empty)
    describe "cappedM" $ do
      let original = FT.Single (Item (Max (21 :: Int)) (Segment 21 30))
      it "left of" $ do
        cappedM 15 original `shouldBe` FT.Single (Item (Max (21 :: Int)) (Segment 21 30))
      it "overlapping" $ do
        cappedM 25 original `shouldBe` FT.Single (Item (Max (26 :: Int)) (Segment 26 30))
      it "left of" $ do
        cappedM 35 original `shouldBe` FT.Empty

    it "should have require function that checks hedgehog properties" $ do
      require $ property $ do
        x <- forAll (Gen.int Range.constantBounded)
        x === x
