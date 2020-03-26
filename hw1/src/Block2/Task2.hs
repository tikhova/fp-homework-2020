{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Block2.Task2
  ( splitOn
  , joinWith
  ) where

import           Block3.Task2 (NonEmpty (..))

-- | Take a separator element, a list of elements and return non empty list
-- of chuncks of the given one that were separated by the separator.
splitOn :: forall a . Eq a => a -> [a] -> NonEmpty [a]
splitOn x xs = y :| ys
  where
    listSplit :: Eq a => a -> ([a], [[a]]) -> ([a], [[a]])
    listSplit z (cur, acc)
      | x == z    = ([], cur : acc)
      | otherwise = (z : cur, acc)

    (y, ys) = foldr listSplit ([], []) xs

-- | Take a separator element, a non empty list of chuncks and 
-- join the chuncks with the separator into list.
joinWith :: forall a . a -> NonEmpty [a] -> [a]
joinWith x (y :| ys) = foldr listJoin [] (reverse (y : ys))
  where
    listJoin :: [a] -> [a] -> [a]
    listJoin []       acc = acc
    listJoin xs acc       = xs ++ x : acc
