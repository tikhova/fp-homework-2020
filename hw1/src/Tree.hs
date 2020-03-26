{-# LANGUAGE InstanceSigs #-}

module Tree
  ( Tree (..)
  , delete
  , find
  , fromList
  , insert
  , isEmpty
  , size
  ) where

import           NonEmpty     (NonEmpty (..))
import           Data.Maybe   (fromJust)
import           Data.List    (foldr)

-- | Tree representation.
data Tree a = Leaf
            | Node { list  :: NonEmpty a -- list of the same elements
                   , left  :: Tree a     -- left subtree
                   , right :: Tree a     -- right subtree
                   } deriving Show

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


-- | Check if tree is empty.
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Get the amount of all elements in the tree.
size :: Tree a -> Int
size Leaf          = 0
size (Node ne l r) = listLength ne + size l + size r
  where
    listLength :: NonEmpty a -> Int
    listLength (_ :| xs) = 1 + length xs

-- | Insert element into the given tree. 
-- If there is a node with the list of this element add it to the list.
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node (x :| []) Leaf Leaf
insert x tree@(Node (y :| ys) l r)
  | x == y    = tree { list = x :| (y : ys) }
  | x < y     = tree { left = insert x l }
  | otherwise = tree { right = insert x r }

-- | Find element in the given tree. 
-- Return Just of the node it was found in,
-- or Nothing if it wasn't found.
find :: Ord a => a -> Tree a -> Maybe (Tree a)
find _  Leaf = Nothing
find x tree@(Node (y :| _) l r)
  | x == y    = Just tree
  | x < y     = find x l
  | otherwise = find x r

-- | Build tree from the list of it's elements.
fromList ::  Ord a => [a] -> Tree a
fromList = foldr insert Leaf

-- | Delete the node that stores given element.
delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x tree@(Node (y :| _) l r)
  | x == y = case l of
      Leaf -> r
      _    ->
          case r of
              Leaf          -> l
              Node _ Leaf _ -> r { left = l }
              _             -> (delete z tree) { list = newList }
               where
                newList@(z :| _) = fromJust $ smallest r
  | x < y     = tree { left = delete x l }
  | otherwise = tree { right = delete x r }

-- | Find the list of smallest elements in given tree.
smallest :: Tree a -> Maybe (NonEmpty a)
smallest Leaf             = Nothing
smallest (Node ne Leaf _) = Just ne
smallest (Node _  l    _) = smallest l
