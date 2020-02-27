module Task3
  ( s
  , composition
  , identity
  , contraction
  , permutation
  ) where

-- | S combinator
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | Composition function. See also: (.)
composition :: (b -> c) -> (a -> b) -> a -> c
composition f = s (const f)

-- | Identity function. See also: id
identity :: a -> a
identity = s const const

-- | Function that reduct an argument of received function
contraction :: (a -> a -> b) -> a -> b
contraction f = s f id

-- | Function that swaps two received arguments 
permutation :: (a -> b -> c) -> b -> a -> c
permutation f b = s f (const b)
