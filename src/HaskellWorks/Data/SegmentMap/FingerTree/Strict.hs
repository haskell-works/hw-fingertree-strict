{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe                  #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable    #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PriorityQueue.FingerTree
-- Copyright   :  (c) Ross Paterson 2008
-- License     :  BSD-style
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs and functional dependencies)
--
-- Interval maps implemented using the 'FingerTree' type, following
-- section 4.8 of
--
--  * Ralf Hinze and Ross Paterson,
--    \"Finger trees: a simple general-purpose data structure\",
--    /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--    <http://staff.city.ac.uk/~ross/papers/FingerTree.html>
--
-- An amortized running time is given for each operation, with /n/
-- referring to the size of the priority queue.  These bounds hold even
-- in a persistent (shared) setting.
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude".  The ambiguity may be resolved
-- using either qualification or the @hiding@ clause.
--
-----------------------------------------------------------------------------

module HaskellWorks.Data.SegmentMap.FingerTree.Strict (
    -- * Intervals
    Interval(..), point,
    -- * Interval maps
    SegmentMap(..), empty, singleton, insert, union,
    -- * Searching
    search, intersections, dominators
    ) where

import           HaskellWorks.Data.FingerTree.Strict (FingerTree, Measured (..), ViewL (..), ViewR (..), viewl, viewr, (<|), (><), (|>))
import qualified HaskellWorks.Data.FingerTree.Strict as FT

import Control.Applicative ((<$>))
import Data.Foldable       (Foldable (foldMap))
import Data.Monoid
import Data.Traversable    (Traversable (traverse))

----------------------------------
-- 4.8 Application: interval trees
----------------------------------

-- | A closed interval.  The lower bound should be less than or equal
-- to the higher bound.
data Segment v = Segment { low :: !v, high :: !v }
    deriving (Eq, Ord, Show)

-- | An interval in which the lower and upper bounds are equal.
point :: v -> Segment v
point v = Segment v v

data Node v a = Node !(Segment v) !a

instance Functor (Node v) where
    fmap f (Node i x) = Node i (f x)

instance Foldable (Node v) where
    foldMap f (Node _ x) = f x

instance Traversable (Node v) where
    traverse f (Node i x) = Node i <$> f x

instance (Ord v, Monoid v) => Measured v (Node v a) where
    measure (Node i _) = error "TODO"

-- | Map of closed intervals, possibly with duplicates.
-- The 'Foldable' and 'Traversable' instances process the intervals in
-- lexicographical order.
newtype SegmentMap v a = SegmentMap (FingerTree v (Node v a))
-- ordered lexicographically by interval

instance Functor (SegmentMap v) where
    fmap f (SegmentMap t) = SegmentMap (FT.unsafeFmap (fmap f) t)

instance Foldable (SegmentMap v) where
    foldMap f (SegmentMap t) = foldMap (foldMap f) t

instance Traversable (SegmentMap v) where
    traverse f (SegmentMap t) =
        SegmentMap <$> FT.unsafeTraverse (traverse f) t

-- | 'empty' and 'union'.
instance (Ord v) => Monoid (SegmentMap v a) where
    mempty = empty
    mappend = union

-- | /O(1)/.  The empty interval map.
empty :: (Ord v) => SegmentMap v a
empty = SegmentMap FT.empty

-- | /O(1)/.  Interval map with a single entry.
singleton :: (Ord v) => Segment v -> a -> SegmentMap v a
singleton i x = SegmentMap (FT.singleton (Node i x))

{-
capL :: (Ord v, Enum v) => v -> Node v a -> Maybe (Node v a)
capL rilo (Node (Interval lilo lihi) a) = if lihi < rilo
  then Just (Node (Interval lilo (pred rilo)) a)
  else Nothing

capR :: (Ord v, Enum v) => v -> Node v a -> Maybe (Node v a)
capR lihi (Node (Interval rilo rihi) a) = if lihi < rilo
  then Just (Node (Interval (succ lihi) rihi) a)
  else Nothing

-- | /O(log n)/.  Insert an interval into a map.
-- The map may contain duplicate intervals; the new entry will be inserted
-- before any existing entries for the same interval.
update :: forall v a. (Ord v, Enum v) => Interval v -> Maybe a -> SegmentMap v a -> SegmentMap v a
update   (Interval lo hi) _ m | lo > hi = m
update i@(Interval lo hi) mx (SegmentMap t) = case mx of
  Just x  -> SegmentMap (cappedL >< Node i x <| cappedR)
  Nothing -> SegmentMap (cappedL >< cappedR)
  where
    (lt, ys) = FT.split larger       t
    (_ , rt) = FT.split (atleast hi) ys
    cappedL :: FingerTree (IntInterval v) (Node v a)
    cappedL = case viewr lt of
      EmptyR    -> lt
      ltp :> n  -> maybe ltp (ltp |>) (capL lo n)
    cappedR :: FingerTree (IntInterval v) (Node v a)
    cappedR = case viewl rt of
      EmptyL    -> rt
      n :< rtp  -> maybe rtp (<| rtp) (capR hi n)
    larger (IntInterval k _) = k >= i
    larger NoInterval        = error "larger NoInterval"
-}
