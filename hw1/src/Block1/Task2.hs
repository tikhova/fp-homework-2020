{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  ( Nat (..)
  , add
  , divide
  , intToNat
  , isEven
  , natMod
  , mul
  , natToInt
  , sub
  ) where

data Nat = Z | S Nat deriving Show

-- | Sum two Nat numbers.
add :: Nat -> Nat -> Nat
add a Z     = a
add a (S b) = S (add a b)

-- | Multiply two Nat numbers.
mul :: Nat -> Nat -> Nat
mul _ Z     = Z
mul a (S b) = add a (mul a b)

-- | Substract two Nat numbers. Return Z if the difference is negative.
sub :: Nat -> Nat -> Nat
sub Z _         = Z
sub a Z         = a
sub (S a) (S b) = sub a b

-- | Divide two Nat numbers. Throw error if the divider is zero.
divide :: Nat -> Nat -> Nat
divide _ Z = error "Devision by zero"
divide a b =
  if a < b
  then Z
  else S $ divide (sub a b) b

-- | Get mod of two Nat numbers. Throw error if the divider is zero.
natMod :: Nat -> Nat -> Nat
natMod _ Z = error "Division by zero"
natMod a b =
  if a < b
  then a
  else natMod (sub a b) b

-- | Transform Nat to Int.
natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S a) = 1 + natToInt a

-- | Transform Int to Nat.
intToNat :: Int -> Nat
intToNat 0 = Z
intToNat a
  | a > 0     = S $ intToNat $ a - 1
  | otherwise = error "Negative number"

-- | Check if Nat number is even.
isEven :: Nat -> Bool
isEven Z     = True
isEven (S a) = not (isEven a)

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Z Z         = True
  (==) (S a) (S b) = (==) a b
  (==) _ _         = False

instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  compare Z Z         = EQ
  compare Z _         = LT
  compare _ Z         = GT
  compare (S a) (S b) = compare a b


