-- {-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
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
-- Module      :  Data.SegmentMap.Strict
-- Copyright   :  (c) Arbor Networks 2017
-- License     :  BSD-style
-- Maintainer  :  mayhem@arbor.net
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs and functional dependencies)
--
-- Segment maps implemented using the 'FingerTree' type, following
-- section 4.8 of
--
--  * Ralf Hinze and Ross Paterson,
--    \"Finger trees: a simple general-purpose data structure\",
--    /Journal of Functional Programmaxg/ 16:2 (2006) pp 197-217.
--    <http://staff.city.ac.uk/~ross/papers/FingerTree.html>
--
-- An amortized running time is given for each operation, with /n/
-- referring to the size of the map.  These bounds hold even
-- in a persistent (shared) setting.
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude".  The ambiguity may be resolved
-- using either qualification or the @hiding@ clause.
--
-----------------------------------------------------------------------------

module HaskellWorks.Data.SegmentMap.Strict
  ( -- * Segments
    Segment(..), point,
    -- * Segment maps
    SegmentMap(..),
    OrderedMap(..),
    delete,
    empty,
    fromList,
    insert,
    singleton,
    update,
    segmentMapToList,

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
import HaskellWorks.Data.FingerTree.Strict (FingerTree, ViewL (..), ViewR (..), viewl, viewr, (<|), (><))
import HaskellWorks.Data.Item.Strict
import HaskellWorks.Data.Segment.Strict

import qualified HaskellWorks.Data.FingerTree.Strict as FT

infixr 5 >*<

----------------------------------
-- 4.8 Application: segment trees
----------------------------------

-- | Map of closed segments, possibly with duplicates.
-- The 'Foldable' and 'Traversable' instances process the segments in
-- lexicographical order.

newtype OrderedMap k a = OrderedMap (FingerTree k (Item k a)) deriving (Show, Generic, NFData)

newtype SegmentMap k a = SegmentMap (OrderedMap (Max k) (Segment k, a)) deriving (Show, Generic, NFData)

-- ordered lexicographically by segment start

instance Functor (OrderedMap k) where
    fmap f (OrderedMap t) = OrderedMap (FT.unsafeFmap (fmap f) t)

instance Foldable (OrderedMap k) where
    foldMap f (OrderedMap t) = foldMap (foldMap f) t

instance Traversable (OrderedMap k) where
    traverse f (OrderedMap t) = OrderedMap <$> FT.unsafeTraverse (traverse f) t

instance Functor (SegmentMap k) where
    fmap f (SegmentMap t) = SegmentMap (fmap (fmap f) t)

-- instance Foldable (SegmentMap k) where
--     foldMap f (SegmentMap t) = foldMap (foldMap f) t

segmentMapToList :: SegmentMap k a -> [(Segment k, a)]
segmentMapToList (SegmentMap m) = toList m

-- instance Traversable (SegmentMap k) where
--     traverse f (SegmentMap t) =
--         SegmentMap <$> FT.unsafeTraverse (traverse f) t

-- | /O(1)/.  The empty segment map.
empty :: (Ord k, Bounded k) => SegmentMap k a
empty = SegmentMap (OrderedMap FT.empty)

-- | /O(1)/.  Segment map with a single entry.
singleton :: (Bounded k, Ord k) => Segment k -> a -> SegmentMap k a
singleton s@(Segment lo hi) a = SegmentMap $ OrderedMap $ FT.singleton $ Item (Max lo) (s, a)

delete :: forall k a. (Bounded k, Ord k, Enum k, Eq a, Show k, Show a)
       => Segment k
       -> SegmentMap k a
       -> SegmentMap k a
delete = flip update Nothing

insert :: forall k a. (Bounded k, Ord k, Enum k, Eq a, Show k, Show a)
       => Segment k
       -> a
       -> SegmentMap k a
       -> SegmentMap k a
insert s a = update s (Just a)

(>*<) :: (Ord k, Enum k, Bounded k, Eq a)
      => FingerTree (Max k) (Item (Max k) (Segment k, a))
      -> FingerTree (Max k) (Item (Max k) (Segment k, a))
      -> FingerTree (Max k) (Item (Max k) (Segment k, a))
(>*<) lt rt = case viewr lt of
  EmptyR          -> rt
  treeL :> Item _ (Segment loL hiL, itemL)  -> case viewl rt of
    EmptyL         -> lt
    Item _ (Segment loR hiR, itemR) :< treeR ->
        if succ hiL >= loR && itemL == itemR
          then treeL >< FT.singleton (Item (Max loL) (Segment loL hiR, itemL)) >< treeR
          else lt >< rt

update :: forall k a. (Ord k, Enum k, Bounded k, Eq a, Show k, Show a)
       => Segment k
       -> Maybe a
       -> SegmentMap k a
       -> SegmentMap k a
update (Segment lo hi)   _        m | lo > hi    = m
update _                 Nothing  m              = m
update s@(Segment lo hi) (Just x) (SegmentMap (OrderedMap t)) =
  SegmentMap $ OrderedMap (at >*< bbbb >*< cccc)
  where
    (fstPivotLt, fstPivotRt) = FT.split (>= Max lo) t
    (at, atSurplus) = cappedL lo fstPivotLt
    (zs, remainder) = FT.split (> Max hi) (atSurplus >*< fstPivotRt)
    e = maybe FT.Empty FT.singleton (FT.maybeLast zs >>= capM hi)
    rt = e >*< remainder
    bbbb = FT.singleton (Item (Max lo) (s, x))
    cccc = cappedM hi rt

cappedL :: (Enum k, Ord k, Bounded k, Show k)
  => k
  -> FingerTree (Max k) (Item (Max k) (Segment k, a))
  -> (FingerTree (Max k) (Item (Max k) (Segment k, a)), FingerTree (Max k) (Item (Max k) (Segment k, a)))
cappedL lo t = case viewr t of
  EmptyR      -> (FT.empty, FT.empty)
  ltp :> item -> resolve ltp item
  where resolve ltp (Item _ (Segment lilo lihi, a))
            | lo <= lilo  = (ltp         , FT.empty)
            | lo <  lihi  = (ltp >< lPart, rPart   )
            | lo <= lihi  = (ltp >< lPart, FT.empty)
            | otherwise   = (t           , FT.empty)
          where lPart = FT.singleton (Item (Max lilo) (Segment lilo (pred lo), a))
                rPart = FT.singleton (Item (Max lo  ) (Segment lo   lihi     , a))

cappedM :: (Enum k, Ord k, Bounded k, Show k, Show a)
  => k
  -> FingerTree (Max k) (Item (Max k) (Segment k, a))
  -> FingerTree (Max k) (Item (Max k) (Segment k, a))
cappedM hi t = case viewl t of
  EmptyL   -> t
  n :< rtp -> maybe rtp (<| rtp) (capM hi n)

capM :: (Ord k, Enum k, Show k, Show a)
  => k
  -> Item (Max k) (Segment k, a)
  -> Maybe (Item (Max k) (Segment k, a))
capM lihi n@(Item _ (Segment rilo rihi, a))
  -- let !_ = trace ("lihi: " <> show lihi) lihi in
  -- let !_ = trace ("rilo: " <> show rilo) rilo in
  -- let !_ = trace ("rihi: " <> show rihi) rihi in
  -- let result = case () of
  | lihi < rilo = Just n
  | lihi < rihi = Just $ Item (Max (succ lihi)) (Segment (succ lihi) rihi, a)
  | otherwise   = Nothing
        -- in
  -- let !_ = trace ("result: " <> show result) result in
  -- result

fromList :: (Ord v, Enum v, Eq a, Bounded v, Show v, Show a)
  => [(Segment v, a)]
  -> SegmentMap v a
fromList = foldl' (flip (uncurry insert)) empty
