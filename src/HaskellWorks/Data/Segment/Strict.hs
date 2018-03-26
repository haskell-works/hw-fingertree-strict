{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Segment.Strict where

import Control.DeepSeq                     (NFData)
import GHC.Generics                        (Generic)
import HaskellWorks.Data.FingerTree.Strict

-- | A closed segment.  The lower bound should be less than or equal
-- to the higher bound.
data Segment k = Segment { low :: !k, high :: !k }
    deriving (Eq, Ord, Show, Generic, NFData)

-- | A segment in which the lower and upper bounds are equal.
point :: k -> Segment k
point k = Segment k k

instance (Monoid k) => Measured k (Segment k) where
  measure = low
