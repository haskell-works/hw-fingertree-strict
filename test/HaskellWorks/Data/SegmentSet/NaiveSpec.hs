module HaskellWorks.Data.SegmentSet.NaiveSpec where

import Data.List
import HaskellWorks.Data.SegmentSet.Naive

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

rawIps :: [Segment Int]
rawIps = [Segment 12 20, Segment 1 10, Segment 220 300]

ips :: SegmentSet Int
ips = fromList rawIps

spec :: Spec
spec = describe "App.NaiveIntervalSpec" $ do
  it "should preserve empty" $ fromList (toList (empty :: SegmentSet Int)) `shouldBe` empty
  it "should insert one range" $ toList (update (Segment 11 20) empty) `shouldBe` ([Segment 11 20] :: [Segment Int])
  it "should not change if update is inclusive" $ update (Segment 12 18) ips `shouldBe` ips

  it "should join cross ranges" $ do
      toList (update (Segment 15 250) ips) `shouldBe` [Segment 1 10, Segment  12 300]
      toList (update (Segment  5  15) ips) `shouldBe` [Segment 1 20, Segment 220 300]

  it "should keep sorted list (fromList)" $
    toList (fromList (reverse rawIps)) `shouldBe` sortOn low rawIps

  it "should preserve sorting (update)" $
    let addon = Segment 50 70
    in toList (update addon ips) `shouldBe` sortOn low (addon:rawIps)

  it "should preserve sorring (delete)" $
    toList (remove (Segment 12 20) ips) `shouldBe` sortOn low (filter (\x -> low x /=  12) rawIps)

  it "should remove nothing from empty" $ remove (Segment 1 10) empty `shouldBe` (empty :: SegmentSet Int)

  it "should remove exact segment" $
    toList (remove (Segment 12 20) ips) `shouldBe` [Segment 1 10, Segment 220 300]

  it "should remove inner segment" $
    toList (remove (Segment 250 270) ips) `shouldBe`
      [Segment 1 10, Segment 12 20, Segment 220 249, Segment 271 300]

  it "should remove crossing segment" $
    toList (remove (Segment 5 15) ips) `shouldBe`
      [Segment 1 4, Segment 16 20, Segment 220 300]

  it "should remove everything" $
    remove (Segment 0 1000) ips `shouldBe` empty

  it "should remove leftmost" $ do
    toList (remove (Segment 1 10) ips) `shouldBe` [Segment 12 20, Segment 220 300]
    toList (remove (Segment 1 15) ips) `shouldBe` [Segment 16 20, Segment 220 300]
    toList (remove (Segment 0 15) ips) `shouldBe` [Segment 16 20, Segment 220 300]

  it "should remove rightmost" $ do
    toList (remove (Segment 220 300) ips) `shouldBe` [Segment 1 10, Segment 12 20]
    toList (remove (Segment 200 300) ips) `shouldBe` [Segment 1 10, Segment 12 20]
    toList (remove (Segment  18 300) ips) `shouldBe` [Segment 1 10, Segment 12 17]
    toList (remove (Segment  18 400) ips) `shouldBe` [Segment 1 10, Segment 12 17]

  it "should merge adjacent segments" $ do
    let segments = [Segment 1 1, Segment 2 2] :: [Segment Int]
    toList (fromList segments) `shouldBe` [Segment 1 2]
