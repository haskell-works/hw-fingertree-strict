module HaskellWorks.Data.SegmentSet.Naive
  ( empty
  , fromList
  , remove
  , toList
  , update
  , Segment(..)
  , SegmentSet(..)
  ) where

import Data.Foldable hiding (toList)
import Data.Maybe
import Debug.Trace

import HaskellWorks.Data.Segment.Strict

newtype SegmentSet a = SegmentSet [Segment a] deriving (Show, Eq)

empty :: SegmentSet a
empty = SegmentSet []

fromList :: (Ord a, Enum a) => [Segment a] -> SegmentSet a
fromList = foldr' update empty

toList :: SegmentSet a -> [Segment a]
toList (SegmentSet as) = as

update :: (Ord a, Enum a) => Segment a -> SegmentSet a -> SegmentSet a
update i (SegmentSet as) =
  let (ls, b1, b2, rs)  = splitSegment i as
      i'                = merge b1 i b2
   in SegmentSet $ ls ++ (i':rs)

remove :: (Ord a, Enum a) => Segment a -> SegmentSet a -> SegmentSet a
remove i (SegmentSet as) =
  let (ls, b1, b2, rs) = splitSegment i as
      b1s = maybe [] (minus i) b1
      b2s = maybe [] (minus i) b2
   in SegmentSet $ ls ++ b1s ++ b2s ++ rs

splitSegment :: (Ord a, Enum a) => Segment a -> [Segment a] -> ([Segment a], Maybe (Segment a), Maybe (Segment a), [Segment a])
splitSegment (Segment s e) as = (ls, b1, b2, rs)
  where (ls, xs ) = break (\(Segment x y) -> x >= s || y >= s) as
        (b1, xs') = unconsMergeable (Segment s e) xs
        (_ , rs') = break (\(Segment x y) -> x >= e || y >= e) xs'
        (b2, rs ) = unconsMergeable (Segment s e) rs'
        unconsMergeable ip ips = case uncons' ips of
          (Just b', rs'') | overlapsOrAdjacent ip b' -> (Just b', rs'')
          _               -> (Nothing, ips)

overlapsOrAdjacent :: (Ord a, Enum a) => Segment a -> Segment a -> Bool
overlapsOrAdjacent (Segment s1 e1) (Segment s2 e2) = if s1 <= s2 then succ e1 >= s2 else e2 >= s1

minus :: (Ord a, Enum a) => Segment a -> Segment a -> [Segment a]
minus (Segment s e) (Segment fs fe) =
  let as = if s <= fs then Nothing else Just (Segment  fs      (pred s))
      bs = if e >= fe then Nothing else Just (Segment (succ e)  fe     )
  in catMaybes [as, bs]

merge :: (Ord a, Enum a) => Maybe (Segment a) -> Segment a -> Maybe (Segment a) -> Segment a
merge (Just (Segment sb1 _  )) (Segment s e) (Just (Segment _   eb2)) = Segment (min sb1 s) (max e eb2)
merge Nothing                  (Segment s e) (Just (Segment sb2 eb2)) = Segment (min sb2 s) (max e eb2)
merge (Just (Segment sb1 eb2)) (Segment s e) Nothing                  = Segment (min sb1 s) (max e eb2)
merge _ i _                                                           = i

uncons' :: [a] -> (Maybe a, [a])
uncons' []     = (Nothing, [])
uncons' (a:as) = (Just a , as)
