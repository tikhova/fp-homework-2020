module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function (fix)
-- fix :: (a -> a) -> a

-- | Function that returns infinite list of received element using fix
iterateElement :: a -> [a]
iterateElement = fix addElement
  where
    addElement ::  (a -> [a]) -> a -> [a]
    addElement f x = x : f x

-- | Function that calculates fibonacci number using fix
fibonacci :: Integer -> Integer
fibonacci = fix function
  where
    function :: (Integer -> Integer) -> Integer -> Integer
    function fib n | n <= 1    = n
                   | otherwise = fib (n - 1) + fib (n - 2)

-- | Function that calculates factorial using fix
factorial :: Integer -> Integer
factorial = fix function
  where
    function :: (Integer -> Integer) -> Integer -> Integer
    function fact n | n == 0    = n
                    | otherwise = n * fact (n - 1)

-- | Function that maps list using given function and fix. See also: map
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix . function
  where
    function :: (a -> b) -> ([a] -> [b]) -> [a] -> [b]
    function _ _ []          = []
    function mapF f (x : xs) = mapF x : function mapF f xs
