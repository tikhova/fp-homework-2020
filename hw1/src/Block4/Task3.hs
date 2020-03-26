{-# LANGUAGE InstanceSigs #-}

module Block4.Task3 (NonEmpty (..)) where

import           Control.Applicative (liftA2)

data NonEmpty a = a :| [a] deriving Show

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
