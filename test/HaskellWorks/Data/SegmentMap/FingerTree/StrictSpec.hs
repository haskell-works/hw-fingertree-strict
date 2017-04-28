{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.SegmentMap.FingerTree.StrictSpec
  ( spec
  ) where

import Data.Foldable

import HaskellWorks.Data.SegmentMap.FingerTree.Strict
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.SegmentMap.FingerTree.Strict as S (fromList)
import qualified Hedgehog.Gen                                   as Gen
import qualified Hedgehog.Range                                 as Range

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.SegmentMap.StrictSpec" $ do
    it "should convert SegmentMap to List" $ do
      let emptySM :: SegmentMap Int Int = empty
      toList emptySM `shouldBe` []

    it "should convert List to SegmentMap" $ do
      let emptySM :: SegmentMap Int Int = empty
      let emptySM2 :: SegmentMap Int Int = S.fromList []
      toList emptySM2 `shouldBe` toList emptySM

    it "should have require function that checks hedgehog properties" $ do
      require $ property $ do
        x <- forAll (Gen.int Range.constantBounded)
        x === x
