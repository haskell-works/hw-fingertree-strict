{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SegmentSet.Strict
-- Copyright   :  (c) Arbor Networks 2017
-- License     :  BSD-style
-- Maintainer  :  mayhem@arbor.net
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs and functional dependencies)
--
-- SegmentSet provides an efficient implementation of a set of segments (a.k.a
-- intervals). Segments in the set are non-overlapping. Adjacent segments
-- are merged (i.e. (a .. b), (b + 1 .. c) -> (a .. c)).
--
-- Segment sets are implemented using the 'FingerTree' type, following
-- section 4.8 of
--
--  * Ralf Hinze and Ross Paterson,
--    \"Finger trees: a simple general-purpose data structure\",
--    /Journal of Functional Programmaxg/ 16:2 (2006) pp 197-217.
--    <http://staff.city.ac.uk/~ross/papers/FingerTree.html>
--
-- An amortized running time is given for each operation, with /n/
-- referring to the size of the set.  These bounds hold even
-- in a persistent (shared) setting.
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude".  The ambiguity may be resolved
-- using either qualification or the @hiding@ clause.
--
-----------------------------------------------------------------------------

module HaskellWorks.Data.SegmentSet.Strict
  ( -- * Segments
    Segment(..), point,
    -- * Segment maps
    SegmentSet(..),
    OrderedMap(..),
    delete,
    empty,
    fromList,
    insert,
    singleton,
    update,
    segmentSetToList,

    Item(..),
    cappedL,
    cappedM
    ) where

import Control.Applicative                 ((<$>))
import Control.DeepSeq                     (NFData)
import Data.Foldable                       (Foldable (foldMap), foldl', toList)
import Data.Semigroup
import Data.Traversable                    (Traversable (traverse))
import GHC.Generics                        (Generic)
import HaskellWorks.Data.FingerTree.Strict (FingerTree, Measured (..), ViewL (..), ViewR (..), viewl, viewr, (<|), (><))
import HaskellWorks.Data.Item.Strict
import HaskellWorks.Data.Segment.Strict

import qualified HaskellWorks.Data.FingerTree.Strict as FT

{- HLINT ignore "Reduce duplication"  -}

infixr 5 >*<

----------------------------------
-- 4.8 Application: segment trees
----------------------------------

-- | Map of closed segments, possibly with duplicates.
-- The 'Foldable' and 'Traversable' instances process the segments in
-- lexicographical order.

newtype OrderedMap k a = OrderedMap (FingerTree k (Item k a)) deriving (Show, Generic, NFData)

newtype SegmentSet k = SegmentSet (OrderedMap (Max k) (Segment k)) deriving (Show, Generic, NFData)

-- ordered lexicographically by segment start

instance Functor (OrderedMap k) where
    fmap f (OrderedMap t) = OrderedMap (FT.unsafeFmap (fmap f) t)

instance Foldable (OrderedMap k) where
    foldMap f (OrderedMap t) = foldMap (foldMap f) t

instance Traversable (OrderedMap k) where
    traverse f (OrderedMap t) = OrderedMap <$> FT.unsafeTraverse (traverse f) t

-- instance Foldable (SegmentSet k) where
--     foldMap f (SegmentSet t) = foldMap (foldMap f) t

segmentSetToList :: SegmentSet k -> [Segment k]
segmentSetToList (SegmentSet m) = toList m

-- instance Traversable (SegmentSet k) where
--     traverse f (SegmentSet t) =
--         SegmentSet <$> FT.unsafeTraverse (traverse f) t

-- | /O(1)/.  The empty segment set.
empty :: SegmentSet k
empty = SegmentSet (OrderedMap FT.empty)

-- | /O(1)/.  Segment set with a single entry.
singleton :: Segment k -> SegmentSet k
singleton s@(Segment lo hi) = SegmentSet $ OrderedMap $ FT.singleton $ Item (Max lo) s

-- | /O(log(n))/. Remove a segment from the set.
-- Alias of update.
delete :: forall k a. (Bounded k, Ord k, Enum k, Show k)
       => Segment k
       -> SegmentSet k
       -> SegmentSet k
delete = flip update False

-- | /O(log(n))/. Insert a segment into the set.
-- Alias of update.
insert :: forall k a. (Bounded k, Ord k, Enum k, Show k)
       => Segment k
       -> SegmentSet k
       -> SegmentSet k
insert = flip update True

-- | Update a segment set. Prefer `insert` or `delete` in most cases.
update :: forall k a. (Ord k, Enum k, Bounded k, Show k)
       => Segment k
       -> Bool
       -> SegmentSet k
       -> SegmentSet k
update (Segment lo hi)   _  m | lo > hi                = m
update s@(Segment lo hi) b (SegmentSet (OrderedMap t)) =
  SegmentSet $ OrderedMap contents
  where
    contents = if b then at >*< bbbb >*< cccc else at >*< cccc
    (fstPivotLt, fstPivotRt) = FT.split (>= Max lo) t
    (at, atSurplus) = cappedL lo fstPivotLt
    (zs, remainder) = FT.split (> Max hi) (atSurplus >*< fstPivotRt)
    e = maybe FT.Empty FT.singleton (FT.maybeLast zs >>= capM hi)
    rt = e >< remainder
    cccc = cappedM hi rt
    bbbb = FT.singleton (Item (Max lo) s)

cappedL :: (Enum k, Ord k, Bounded k, Show k)
  => k
  -> FingerTree (Max k) (Item (Max k) (Segment k))
  -> (FingerTree (Max k) (Item (Max k) (Segment k)), FingerTree (Max k) (Item (Max k) (Segment k)))
cappedL lo t = case viewr t of
  EmptyR      -> (FT.empty, FT.empty)
  ltp :> item -> resolve ltp item
  where resolve ltp (Item _ (Segment lilo lihi))
            | lo <= lilo  = (ltp         , FT.empty)
            | lo <  lihi  = (ltp >< lPart, rPart   )
            | lo <= lihi  = (ltp >< lPart, FT.empty)
            | otherwise   = (t           , FT.empty)
          where lPart = FT.singleton (Item (Max lilo) (Segment lilo (pred lo)))
                rPart = FT.singleton (Item (Max lo  ) (Segment lo   lihi     ))

cappedM :: (Enum k, Ord k, Bounded k, Show k)
  => k
  -> FingerTree (Max k) (Item (Max k) (Segment k))
  -> FingerTree (Max k) (Item (Max k) (Segment k))
cappedM hi t = case viewl t of
  EmptyL   -> t
  n :< rtp -> maybe rtp (<| rtp) (capM hi n)

capM :: (Ord k, Enum k, Show k)
  => k
  -> Item (Max k) (Segment k)
  -> Maybe (Item (Max k) (Segment k))
capM lihi n@(Item _ (Segment rilo rihi))
  | lihi < rilo = Just n
  | lihi < rihi = Just $ Item (Max (succ lihi)) (Segment (succ lihi) rihi)
  | otherwise   = Nothing

fromList :: (Ord v, Enum v, Bounded v, Show v)
  => [Segment v]
  -> SegmentSet v
fromList = foldl' (flip insert) empty

--------------------------------------------------------------------------------
-- Private functions
--------------------------------------------------------------------------------

-- | /O(log(n))/. Merge two segment sets.
-- Private (bare) function to merge two segment sets.
-- Requires two guarantees from the caller:
-- 1) That the sets are non-overlapping, and
-- 2) That the left tree is "less" than the right tree. i.e. that the maximum
-- high in the left tree is less than the minimum low in the right tree.
-- If the two middle-most segments are adjacent:
--   (max (hi left) == succ (min (low right))
-- then those two segments will be merged.
merge :: (Ord k, Enum k, Bounded k)
       => FingerTree (Max k) (Item (Max k) (Segment k))
       -> FingerTree (Max k) (Item (Max k) (Segment k))
       -> FingerTree (Max k) (Item (Max k) (Segment k))
merge lt rt = case viewr lt of
  EmptyR          -> rt
  treeL :> Item _ (Segment loL hiL)  -> case viewl rt of
    EmptyL         -> lt
    Item _ (Segment loR hiR) :< treeR ->
        if succ hiL >= loR
          then treeL >< FT.singleton (Item (Max loL) (Segment loL hiR)) >< treeR
          else lt >< rt

-- | Operator version of merge.
(>*<) :: (Ord k, Enum k, Bounded k)
      => FingerTree (Max k) (Item (Max k) (Segment k))
      -> FingerTree (Max k) (Item (Max k) (Segment k))
      -> FingerTree (Max k) (Item (Max k) (Segment k))
(>*<) = merge
