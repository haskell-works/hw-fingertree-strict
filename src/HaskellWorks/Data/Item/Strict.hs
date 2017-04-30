{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Item.Strict where

import HaskellWorks.Data.FingerTree.Strict

data Item k a = Item !k !a deriving (Eq, Show)

instance Functor (Item k) where
    fmap f (Item i t) = Item i (f t)

instance Foldable (Item k) where
    foldMap f (Item _ x) = f x

instance Traversable (Item k) where
    traverse f (Item i x) = Item i <$> f x

instance (Monoid k) => Measured k (Item k a) where
  measure (Item k _) = k
