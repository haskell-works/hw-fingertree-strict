{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
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
    -- * Segments
    Segment(..), point,
    -- * Segment maps
    SegmentMap(..), empty, singleton
    ) where

import           HaskellWorks.Data.FingerTree.Strict (FingerTree, Measured (..), ViewL (..), ViewR (..), viewl, viewr, (<|), (><), (|>))
import qualified HaskellWorks.Data.FingerTree.Strict as FT

import Control.Applicative ((<$>))
import Data.Foldable       (Foldable (foldMap))
import Data.Traversable    (Traversable (traverse))

----------------------------------
-- 4.8 Application: segment trees
----------------------------------

-- | A closed segment.  The lower bound should be less than or equal
-- to the higher bound.
data Segment k = Segment { low :: !k, high :: !k }
    deriving (Eq, Ord, Show)

-- | An segment in which the lower and upper bounds are equal.
point :: k -> Segment k
point k = Segment k k

data Node k a = Node !(Segment k) !a

instance Functor (Node k) where
    fmap f (Node i t) = Node i (f t)

instance Foldable (Node k) where
    foldMap f (Node _ x) = f x

-- instance Traversable (Node k) where
--     traverse f (Node i x) = Node i <$> f x

instance (Monoid k) => Measured k (Segment k) where
  measure = low

instance (Monoid k) => Measured k (Node k a) where
    measure (Node k _) = measure k

-- | Map of closed segments, possibly with duplicates.
-- The 'Foldable' and 'Traversable' instances process the segments in
-- lexicographical order.
newtype SegmentMap k a = SegmentMap (FingerTree k (Node k a))
-- ordered lexicographically by segment start

instance Functor (SegmentMap k) where
    fmap f (SegmentMap t) = SegmentMap (FT.unsafeFmap (fmap f) t)

instance Foldable (SegmentMap k) where
    foldMap f (SegmentMap t) = foldMap (foldMap f) t

-- instance Traversable (SegmentMap k) where
--     traverse f (SegmentMap t) =
--         SegmentMap <$> FT.unsafeTraverse (traverse f) t

-- -- | 'empty' and 'union'.
-- instance (Ord k) => Monoid (SegmentMap k a) where
--     mempty = empty
--     mappend = union

-- | /O(1)/.  The empty segment map.
empty :: (Ord k, Monoid k) => SegmentMap k a
empty = SegmentMap FT.empty

-- | /O(1)/.  Interval map with a single entry.
singleton :: (Ord k, Monoid k) => Segment k -> a -> SegmentMap k a
singleton s@(Segment lo hi) a = SegmentMap $ FT.singleton $ Node s a

update :: forall k a. (Monoid k, Ord k, Enum k, Eq a)
       => Segment k
       -> Maybe a
       -> SegmentMap k a
       -> SegmentMap k a
update (Segment lo hi)   _        m | lo > hi    = m
update _                 Nothing  m              = m
update s@(Segment lo hi) (Just x) (SegmentMap t) =
  SegmentMap $ cappedL lo lt >< Node s x <| cappedR hi rt
  where
    (lt, ys) = FT.split (>= lo) t
    (_, rt)  = FT.split (> hi) ys

cappedL :: (Enum k, Monoid k, Ord k) => k -> FingerTree k (Node k a) -> FingerTree k (Node k a)
cappedL lo t = case viewr t of
  EmptyR   -> t
  ltp :> n -> maybe ltp (ltp |>) (capL lo n)

cappedR :: (Enum k, Monoid k, Ord k) => k -> FingerTree k (Node k a) -> FingerTree k (Node k a)
cappedR hi t = case viewl t of
  EmptyL   -> t
  n :< rtp -> maybe rtp (<| rtp) (capR hi n)

capL :: (Ord k, Enum k) => k -> Node k a -> Maybe (Node k a)
capL rilo (Node (Segment lilo lihi) a) = if lihi < rilo
  then Just $ Node (Segment lilo (pred rilo)) a
  else Nothing

capR :: (Ord k, Enum k) => k -> Node k a -> Maybe (Node k a)
capR lihi (Node (Segment rilo rihi) a) = if lihi < rilo
  then Just $ Node (Segment (succ lihi) rihi) a
  else Nothing

fromList :: (Monoid v, Ord v, Enum v, Eq a) => [(Segment v, Maybe a)] -> SegmentMap v a
fromList = foldr (uncurry update) empty

{-
capL :: (Ord k, Enum k) => k -> Node k a -> Maybe (Node k a)
capL rilo (Node (Segment lilo lihi) a) = if lihi < rilo
  then Just (Node (Segment lilo (pred rilo)) a)
  else Nothing

capR :: (Ord k, Enum k) => k -> Node k a -> Maybe (Node k a)
capR lihi (Node (Segment rilo rihi) a) = if lihi < rilo
  then Just (Node (Segment (succ lihi) rihi) a)
  else Nothing

-- | /O(log n)/.  Insert an Segment into a map.
-- The map may contain duplicate Segments; the new entry will be inserted
-- before any existing entries for the same Segment.
update :: forall k a. (Ord k, Enum k) => Segment k -> Maybe a -> SegmentMap k a -> SegmentMap k a
update   (Segment lo hi) _ m | lo > hi = m
update i@(Segment lo hi) mx (SegmentMap t) = case mx of
  Just x  -> SegmentMap (cappedL >< Node i x <| cappedR)
  Nothing -> SegmentMap (cappedL >< cappedR)
  where
    (lt, ys) = FT.split larger       t
    (_ , rt) = FT.split (atleast hi) ys
    cappedL :: FingerTree (IntSegment k) (Node k a)
    cappedL = case viewr lt of
      EmptyR    -> lt
      ltp :> n  -> maybe ltp (ltp |>) (capL lo n)
    cappedR :: FingerTree (IntSegment k) (Node k a)
    cappedR = case viewl rt of
      EmptyL    -> rt
      n :< rtp  -> maybe rtp (<| rtp) (capR hi n)
    larger (IntSegment k _) = k >= i
    larger NoSegment        = error "larger NoInterval"
-}
