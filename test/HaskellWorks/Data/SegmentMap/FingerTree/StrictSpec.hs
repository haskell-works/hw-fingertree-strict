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

import Debug.Trace

import HaskellWorks.Data.FingerTree.Strict (ViewR(..), ViewL (..), viewr, viewl, (<|), (|>), (><))

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
              (lt, ys) = FT.split (>= Max lo) t
              (_, rt)  = FT.split (> Max hi) ys
              resl = cappedL lo lt
              resm = Node (Max lo) (s, x)
              resr = cappedR hi rt
              result = SegmentMap $ OrderedMap (resl >< resm <| resr)
              in do
          putStrLn $ "t: " <> show t
          putStrLn $ "lo: " <> show lo
          putStrLn $ "lt: " <> show lt
          putStrLn $ "ys: " <> show ys
          putStrLn $ "rt: " <> show rt
          putStrLn $ "resl: " <> show resl
          putStrLn $ "resm: " <> show resm
          putStrLn $ "resr: " <> show resr
          print result
      print expected
      segmentMapToList updated `shouldBe` expected

    describe "cappedL" $ do
      let original = FT.Single (Node (Max (11  :: Int)) (Segment {low = 11 :: Int, high = 20}, "A" :: String))
      it "left of" $ do
        cappedL  5 original `shouldBe` FT.Empty
      it "overlapping" $ do
        cappedL 15 original `shouldBe` FT.Single (Node (Max (11  :: Int)) (Segment {low = 11 :: Int, high = 14}, "A" :: String))
      it "right of" $ do
        cappedL 25 original `shouldBe` FT.Single (Node (Max (11  :: Int)) (Segment {low = 11 :: Int, high = 20}, "A" :: String))
    describe "cappedR" $ do
      let original = FT.Single (Node (Max (21 :: Int)) (Segment {low = 21 :: Int, high = 30}, "C" :: String))
      it "left of" $ do
        cappedR 15 original `shouldBe` FT.Single (Node (Max (21 :: Int)) (Segment {low = 21 :: Int, high = 30}, "C" :: String))
      it "overlapping" $ do
        cappedR 25 original `shouldBe` FT.Single (Node (Max (26 :: Int)) (Segment {low = 26 :: Int, high = 30}, "C" :: String))
      it "left of" $ do
        cappedR 35 original `shouldBe` FT.Empty

    it "should have require function that checks hedgehog properties" $ do
      require $ property $ do
        x <- forAll (Gen.int Range.constantBounded)
        x === x
