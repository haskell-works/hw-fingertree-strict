{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import           HaskellWorks.Data.FingerTree.Strict (FingerTree, Measured (..), ViewL (..), (<|), (><))
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
data Interval v = Interval { low :: !v, high :: !v }
    deriving (Eq, Ord, Show)

-- | An interval in which the lower and upper bounds are equal.
point :: v -> Interval v
point v = Interval v v

data Node v a = Node !(Interval v) !a

instance Functor (Node v) where
    fmap f (Node i x) = Node i (f x)

instance Foldable (Node v) where
    foldMap f (Node _ x) = f x

instance Traversable (Node v) where
    traverse f (Node i x) = Node i <$> f x

-- rightmost interval (including largest lower bound) and largest upper bound.
data IntInterval v = NoInterval | IntInterval !(Interval v) !v

instance Ord v => Monoid (IntInterval v) where
    mempty = NoInterval
    NoInterval `mappend` i  = i
    i `mappend` NoInterval  = i
    IntInterval _ hi1 `mappend` IntInterval int2 hi2 =
        IntInterval int2 (max hi1 hi2)

instance (Ord v) => Measured (IntInterval v) (Node v a) where
    measure (Node i _) = IntInterval i (high i)

-- | Map of closed intervals, possibly with duplicates.
-- The 'Foldable' and 'Traversable' instances process the intervals in
-- lexicographical order.
newtype SegmentMap v a =
    SegmentMap (FingerTree (IntInterval v) (Node v a))
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
singleton :: (Ord v) => Interval v -> a -> SegmentMap v a
singleton i x = SegmentMap (FT.singleton (Node i x))

-- | /O(log n)/.  Insert an interval into a map.
-- The map may contain duplicate intervals; the new entry will be inserted
-- before any existing entries for the same interval.
insert :: (Ord v) => Interval v -> a -> SegmentMap v a -> SegmentMap v a
insert (Interval lo hi) _ m | lo > hi = m
insert i x (SegmentMap t) = SegmentMap (l >< Node i x <| r)
  where
    (l, r) = FT.split larger t
    larger (IntInterval k _) = k >= i
    larger NoInterval        = error "larger NoInterval"

-- | /O(m log (n/\//m))/.  Merge two interval maps.
-- The map may contain duplicate intervals; entries with equal intervals
-- are kept in the original order.
union  ::  (Ord v) => SegmentMap v a -> SegmentMap v a -> SegmentMap v a
union (SegmentMap xs) (SegmentMap ys) = SegmentMap (merge1 xs ys)
  where
    merge1 as bs = case FT.viewl as of
        EmptyL                  -> bs
        a@(Node i _) :< as'     -> l >< a <| merge2 as' r
          where
            (l, r) = FT.split larger bs
            larger (IntInterval k _) = k >= i
            larger NoInterval        = error "larger NoInterval"
    merge2 as bs = case FT.viewl bs of
        EmptyL                  -> as
        b@(Node i _) :< bs'     -> l >< b <| merge1 r bs'
          where
            (l, r) = FT.split larger as
            larger (IntInterval k _) = k > i
            larger NoInterval        = error "larger NoInterval"

-- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- interval, in lexicographical order.
intersections :: (Ord v) => Interval v -> SegmentMap v a -> [(Interval v, a)]
intersections i = inRange (low i) (high i)

-- | /O(k log (n/\//k))/.  All intervals that contain the given interval,
-- in lexicographical order.
dominators :: (Ord v) => Interval v -> SegmentMap v a -> [(Interval v, a)]
dominators i = inRange (high i) (low i)

-- | /O(k log (n/\//k))/.  All intervals that contain the given point,
-- in lexicographical order.
search :: (Ord v) => v -> SegmentMap v a -> [(Interval v, a)]
search p = inRange p p

-- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- interval, in lexicographical order.
inRange :: (Ord v) => v -> v -> SegmentMap v a -> [(Interval v, a)]
inRange lo hi (SegmentMap t) = matches (FT.takeUntil (greater hi) t)
  where
    matches xs  =  case FT.viewl (FT.dropUntil (atleast lo) xs) of
        EmptyL          ->  []
        Node i x :< xs' ->  (i, x) : matches xs'

atleast :: (Ord v) => v -> IntInterval v -> Bool
atleast k (IntInterval _ hi) = k <= hi
atleast _ NoInterval         = error "atleast NoInterval"

greater :: (Ord v) => v -> IntInterval v -> Bool
greater k (IntInterval i _) = low i > k
greater _ NoInterval        = error "greater NoInterval"

{-
-- Examples

mkMap :: (Ord v) => [(v, v, a)] -> SegmentMap v a
mkMap = foldr ins empty
  where
    ins (lo, hi, n) = insert (Interval lo hi) n

composers :: SegmentMap Int String
composers = mkMap [
    (1685, 1750, "Bach"),
    (1685, 1759, "Handel"),
    (1732, 1809, "Haydn"),
    (1756, 1791, "Mozart"),
    (1770, 1827, "Beethoven"),
    (1782, 1840, "Paganini"),
    (1797, 1828, "Schubert"),
    (1803, 1869, "Berlioz"),
    (1810, 1849, "Chopin"),
    (1833, 1897, "Brahms"),
    (1838, 1875, "Bizet")]

mathematicians :: SegmentMap Int String
mathematicians = mkMap [
    (1642, 1727, "Newton"),
    (1646, 1716, "Leibniz"),
    (1707, 1783, "Euler"),
    (1736, 1813, "Lagrange"),
    (1777, 1855, "Gauss"),
    (1811, 1831, "Galois")]
-}
