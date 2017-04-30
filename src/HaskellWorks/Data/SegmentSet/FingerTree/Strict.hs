{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
#if __GLASGOW_HASKELL__ >= 702
-- {-# LANGUAGE Safe                  #-}
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
--    /Journal of Functional Programmaxg/ 16:2 (2006) pp 197-217.
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

module HaskellWorks.Data.SegmentSet.FingerTree.Strict
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

import           HaskellWorks.Data.FingerTree.Strict (FingerTree, Measured (..), ViewL (..), ViewR (..), viewl, viewr, (<|), (><))
import qualified HaskellWorks.Data.FingerTree.Strict as FT

import Control.Applicative ((<$>))
import Data.Foldable       (Foldable (foldMap), toList)
import Data.Semigroup
import Data.Traversable    (Traversable (traverse))

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

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

data Item k a = Item !k !a deriving (Eq, Show)

instance Functor (Item k) where
    fmap f (Item i t) = Item i (f t)

instance Foldable (Item k) where
    foldMap f (Item _ x) = f x

instance Traversable (Item k) where
    traverse f (Item i x) = Item i <$> f x

instance (Monoid k) => Measured k (Segment k) where
  measure = low

instance (Monoid k) => Measured k (Item k a) where
  measure (Item k _) = k

-- | Map of closed segments, possibly with duplicates.
-- The 'Foldable' and 'Traversable' instances process the segments in
-- lexicographical order.

newtype OrderedMap k a = OrderedMap (FingerTree k (Item k a)) deriving Show

newtype SegmentSet k = SegmentSet (OrderedMap (Max k) (Segment k)) deriving Show

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

-- | /O(1)/.  The empty segment map.
empty :: (Ord k, Bounded k) => SegmentSet k
empty = SegmentSet (OrderedMap FT.empty)

-- | /O(1)/.  Segment map with a single entry.
singleton :: (Bounded k, Ord k) => Segment k -> a -> SegmentSet k
singleton s@(Segment lo hi) a = SegmentSet $ OrderedMap $ FT.singleton $ Item (Max lo) s

delete :: forall k a. (Bounded k, Ord k, Enum k, Show k)
       => Segment k
       -> SegmentSet k
       -> SegmentSet k
delete = flip update False

insert :: forall k a. (Bounded k, Ord k, Enum k, Show k)
       => Segment k
       -> SegmentSet k
       -> SegmentSet k
insert s = update s True

(>*<) :: (Ord k, Enum k, Bounded k)
      => FingerTree (Max k) (Item (Max k) (Segment k))
      -> FingerTree (Max k) (Item (Max k) (Segment k))
      -> FingerTree (Max k) (Item (Max k) (Segment k))
(>*<) lt rt = case viewr lt of
  EmptyR          -> rt
  treeL :> Item _ (Segment loL hiL)  -> case viewl rt of
    EmptyL         -> lt
    Item _ (Segment loR hiR) :< treeR ->
        if succ hiL >= loR
          then treeL >< FT.singleton (Item (Max loL) (Segment loL hiR)) >< treeR
          else lt >< rt

update :: forall k a. (Ord k, Enum k, Bounded k, Show k)
       => Segment k
       -> Bool
       -> SegmentSet k
       -> SegmentSet k
update (Segment lo hi) _      m | lo > hi    = m
update (Segment lo hi) False  (SegmentSet (OrderedMap t)) =
  SegmentSet $ OrderedMap (at >*< cccc)
  where
    (fstPivotLt, fstPivotRt) = FT.split (>= Max lo) t
    (at, atSurplus) = cappedL lo fstPivotLt
    (zs, remainder) = FT.split (> Max hi) (atSurplus >*< fstPivotRt)
    e = maybe FT.Empty FT.singleton (FT.maybeLast zs >>= capM hi)
    rt = e >< remainder
    cccc = cappedM hi rt
update s@(Segment lo hi) True   (SegmentSet (OrderedMap t)) =
  SegmentSet $ OrderedMap (at >*< bbbb >*< cccc)
  where
    (fstPivotLt, fstPivotRt) = FT.split (>= Max lo) t
    (at, atSurplus) = cappedL lo fstPivotLt
    (zs, remainder) = FT.split (> Max hi) (atSurplus >*< fstPivotRt)
    e = maybe FT.Empty FT.singleton (FT.maybeLast zs >>= capM hi)
    rt = e >< remainder
    bbbb = FT.singleton (Item (Max lo) s)
    cccc = cappedM hi rt

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
fromList = foldl (flip insert) empty
