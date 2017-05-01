module HaskellWorks.Data.Gen
  ( genSegment
  , genSegments
  , genIntSegment
  , genOrderedIntSegments
  ) where

import Data.List
import HaskellWorks.Data.Segment.Strict

import Hedgehog.Gen (Gen (..))

import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

pairs :: [a] -> [(a, a)]
pairs (a:b:rs) = (a, b):pairs rs
pairs _        = []

unsafeNub :: Eq a => [a] -> [a]
unsafeNub (a:b:bs) = if a == b then a:bs else a:unsafeNub (b:bs)
unsafeNub (a:as)   = a:as
unsafeNub []       = []

genSegment :: Monad m => Gen m (Segment Int)
genSegment = do
    lt <- Gen.int (Range.linear 0 1000)
    rt <- Gen.int (Range.linear lt 1000)
    return $ Segment lt rt

genSegments :: Monad m => Gen m [Segment Int]
genSegments = Gen.list (Range.linear 0 100) genSegment

genIntSegment :: Monad m => Int -> Int -> Gen m (Segment Int)
genIntSegment a b = do
  a <- Gen.int (Range.linear a b)
  b <- Gen.int (Range.linear a b)
  return (Segment (a `min` b) (a `max` b))

genOrderedIntSegments :: Monad m => Int -> Int -> Int -> Gen m [Segment Int]
genOrderedIntSegments n a b = do
  as <- Gen.list (Range.linear 0 (n * 2)) (Gen.int (Range.linear a b))
  return $ unsafeNub (uncurry Segment <$> pairs (sort as))
