{-# LANGUAGE InstanceSigs #-}

module Block6.Task1
  ( Parser (..)
  , first
  ) where

import           Control.Applicative (Alternative, empty, (<|>))

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

-- | Performs function on the first element of the given pair.
-- Returns pair with unchanged second element as a result.
first 
   :: (a -> c) -- function
   -> (a, b)   -- initial pair
   -> (c, b)   -- result pair
first f (x, s) = (f x, s)

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f p = p { runParser = fmap (first f) . runParser p }

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser { runParser = Just . (,) x }

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (Parser fm) (Parser fa)  = Parser $ \s ->
    case fm s of
      Nothing      -> Nothing
      Just (f, s') -> fmap (first f) (fa s')

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser p1) (Parser p2) = Parser $ \s -> p1 s <|> p2 s

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser pa) f = Parser $ \s ->
    case pa s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (f a) s'
