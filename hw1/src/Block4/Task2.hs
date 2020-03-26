{-# LANGUAGE InstanceSigs #-}

module Block4.Task2 (Tree (..)) where

import           Control.Applicative (liftA2)

-- | Tree representation.
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Branch a b) = Branch (fmap f a) (fmap f b)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf x) t     = fmap x t
  (<*>) (Branch x y) t = Branch (x <*> t) (y <*> t)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z (Leaf x)     = f x z
  foldr f z (Branch x y) = x'
    where
      x' = foldr f y' x
      y' = foldr f z y

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Branch x y) = liftA2 Branch (traverse f x) (traverse f y)
