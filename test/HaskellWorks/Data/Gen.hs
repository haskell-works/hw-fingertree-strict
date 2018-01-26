module HaskellWorks.Data.Gen
  ( genSegments
  , genIntSegment
  , genOrderedIntSegments
  ) where

import Data.List
import HaskellWorks.Data.Segment.Strict
import Hedgehog                         (MonadGen)

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

pairs :: [a] -> [(a, a)]
pairs (a:b:rs) = (a, b):pairs rs
pairs _        = []

unsafeNub :: Eq a => [a] -> [a]
unsafeNub (a:b:bs) = if a == b then a:bs else a:unsafeNub (b:bs)
unsafeNub (a:as)   = a:as
unsafeNub []       = []

genSegment :: MonadGen m => m (Segment Int)
genSegment = do
    lt <- G.int (R.linear 0  1000)
    rt <- G.int (R.linear lt 1000)
    return $ Segment lt rt

genSegments :: MonadGen m => Int -> Int -> Int -> m [Segment Int]
genSegments len minInt maxInt = G.list (R.linear 0 len) $ genIntSegment minInt maxInt

genIntSegment :: MonadGen m => Int -> Int -> m (Segment Int)
genIntSegment minInt maxInt = do
  a <- G.int (R.linear minInt maxInt)
  b <- G.int (R.linear minInt maxInt)
  return (Segment (a `min` b) (a `max` b))

genOrderedIntSegments :: MonadGen m => Int -> Int -> Int -> m [Segment Int]
genOrderedIntSegments n minInt maxInt = do
  as <- G.list (R.linear 0 (n * 2)) (G.int (R.linear minInt maxInt))
  return $ unsafeNub (uncurry Segment <$> pairs (sort as))
