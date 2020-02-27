module Task5
  ( zero
  , succChurch
  , churchPlus
  , churchMult
  , churchToInt
  ) where

type Nat a = (a -> a) -> a -> a

-- | Church zero
zero :: Nat a
zero _ x = x

-- | Church successor
succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

-- | Church plus
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b f x = a f (b f x)

-- | Church multiply
churchMult :: Nat a -> Nat a -> Nat a
churchMult a b f = a (b f)

-- | Function transforming Church numeral to Integer
churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0
