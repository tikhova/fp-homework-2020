{-# LANGUAGE InstanceSigs #-}

module NonEmpty (NonEmpty (..)) where

import           Control.Applicative (liftA2)

-- | Non empty list representation.
data NonEmpty a = a :| [a] deriving Show

instance Eq a => Eq (NonEmpty a) where
  (==) :: NonEmpty a -> NonEmpty a -> Bool
  (==) (x :| xs) (y :| ys) = x == y && xs == ys

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ y : ys)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| fs) (x :| xs) = f x :| (fs <*> xs)

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs)     = f x xs'
    where
      xs' = foldr f z xs

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = liftA2 (:|) (f x) (traverse f xs)
