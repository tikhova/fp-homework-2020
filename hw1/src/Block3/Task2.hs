{-# LANGUAGE InstanceSigs #-}

module Block3.Task2
  ( Endo (..)
  , Name (..)
  , ThisOrThat (..)
  ) where

data ThisOrThat a b = This a | That b | Both a b deriving Show

instance (Eq a, Eq b) => Eq (ThisOrThat a b) where
  (==) :: ThisOrThat a b -> ThisOrThat a b -> Bool
  (==) (This a)   (This b)     = a == b
  (==) (That a)   (That b)     = a == b
  (==) (Both a b) (Both a' b') = a == a' && b == b'
  (==) _          _            = False

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

-- | Name representation.
newtype Name = Name String deriving Show

instance Eq Name where
  (==) :: Name -> Name -> Bool
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
