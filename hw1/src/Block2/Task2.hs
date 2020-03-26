{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Block2.Task2
  ( splitOn
  , joinWith
  ) where

import           Block3.Task2 (NonEmpty (..))

splitOn :: forall a . Eq a => a -> [a] -> NonEmpty [a]
splitOn x xs = y :| ys
  where
    listSplit :: Eq a => a -> ([a], [[a]]) -> ([a], [[a]])
    listSplit z (cur, acc)
      | x == z    = ([], cur : acc)
      | otherwise = (z : cur, acc)

    (y, ys) = foldr listSplit ([], []) xs

joinWith :: forall a . a -> NonEmpty [a] -> [a]
joinWith x (y :| ys) = foldr listJoin [] (reverse (y : ys))
  where
    listJoin :: [a] -> [a] -> [a]
    listJoin []       acc = acc
    listJoin xs acc       = xs ++ x : acc
