module SegmentMapSpec where

import HaskellWorks.Data.SegmentMap.FingerTree.Strict

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit                           (Assertion, (@?=))
import Test.QuickCheck                      hiding ((><))
import Test.QuickCheck.Poly

import Data.Semigroup(Max)
import Data.Monoid(Max)

main :: IO ()
main = undefined

-- fromList :: [(k, k, a)] -> SegmentMap k a
-- fromList = undefined
--
-- toList :: SegmentMap k a -> [(k, k, a)]
-- toList = undefined

test_update :: Assertion
test_update = toList updated @?= expected
  where
    initial :: SegmentMap (Max Int) String -- Type hint for the compiler.
    initial = fromList [(Max 1, Max 10, "1-10"), (Max 11, Max 20, "11-20")]
    updated = update (Segment (Max 5) (Max 15)) (Just "5-15") initial
    expected = [(Max 1, Max 4, "1-10"), (Max 5, Max 15, "5-15"), (Max 16, Max 20, "10-20")]
