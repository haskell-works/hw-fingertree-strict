module HaskellWorks.Data.Gen where

import Data.List
import HaskellWorks.Data.Segment.Strict

import Hedgehog.Gen   (Gen (..))
import Hedgehog.Range (Range (..))

import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

pairs :: [a] -> [(a, a)]
pairs (a:b:rs) = (a, b):pairs rs
pairs _        = []

unsafeNub :: Eq a => [a] -> [a]
unsafeNub (a:b:bs) = if a == b then a:bs else a:unsafeNub (b:bs)
unsafeNub (a:as)   = a:as
unsafeNub []       = []

intSegment :: Monad m => Int -> Int -> Gen m (Segment Int)
intSegment min max = do
  a <- Gen.int (Range.linear 1   100)
  b <- Gen.int (Range.linear 1   100)
  return (Segment a b)

orderedIntSegments :: Monad m => Int -> Int -> Int -> Gen m [Segment Int]
orderedIntSegments n min max = do
  as <- Gen.list (Range.linear min (n * 2)) (Gen.int (Range.linear min max))
  return $ unsafeNub (uncurry Segment <$> pairs (sort as))
