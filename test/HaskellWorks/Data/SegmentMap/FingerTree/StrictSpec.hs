{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.SegmentMap.FingerTree.StrictSpec
  ( spec
  ) where

import Data.Foldable

import           HaskellWorks.Data.SegmentMap.FingerTree.Strict
import qualified HaskellWorks.Data.SegmentMap.FingerTree.Strict as S (fromList)

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

-- To prevent `No instance for (Monoid Int)`
instance Monoid Int where
    mempty  = 0
    mappend = (+)

spec :: Spec
spec = describe "HaskellWorks.Data.SegmentMap.StrictSpec" $ do
    it "should convert SegmentMap to List" $ do
      let emptySM :: SegmentMap Int Int = empty
      toList emptySM `shouldBe` []

    it "should convert List to SegmentMap" $ do
      let emptySM :: SegmentMap Int Int = empty
      let emptySM2 :: SegmentMap Int Int = S.fromList []
      toList emptySM2 `shouldBe` toList emptySM

