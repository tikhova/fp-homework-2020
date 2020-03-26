{-# LANGUAGE InstanceSigs #-}

module Block2.Task1 where

import           Block1.Task3 (Tree (..))
import           Data.List    (foldr)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf         = z
  foldr f z (Node m l r) = l'
    where
      l' = foldr f m' l
      m' = Data.List.foldr f r' m
      r' = foldr f z r

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f = foldr (mappend . f) mempty
