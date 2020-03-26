{-# LANGUAGE InstanceSigs #-}

module Block3.Task2
  ( Endo (..)
  , Name (..)
  , NonEmpty (..)
  , ThisOrThat (..)
  ) where

data NonEmpty a = a :| [a] deriving Show

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ y : ys)


data ThisOrThat a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) (This a) (That b)       = Both a b
  (<>) (That b) (This a)       = Both a b
  (<>) (This a) (This b)       = This (a <> b)
  (<>) (That a) (That b)       = That (a <> b)
  (<>) (This a') (Both a b)    = Both (a <> a') b
  (<>) (That b') (Both a b)    = Both a (b <> b')
  (<>) (Both a b) (This a')    = Both (a <> a') b
  (<>) (Both a b) (That b')    = Both a (b <> b')
  (<>) (Both a b) (Both a' b') = Both (a <> a') (b <> b')


newtype Name = Name String

instance Eq Name where
  (Name a) == (Name b) = a == b

instance Semigroup Name where
  (<>) (Name a) (Name b) = Name $ a ++ '.' : b

instance Monoid Name where
  mempty = Name ""

  mappend a b
    | a == mempty = b
    | b == mempty = a
    | otherwise   = a <> b


newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  (<>) (Endo a) (Endo b) = Endo $ a . b

instance Monoid (Endo a) where
  mempty = Endo id
  mappend = (<>)
