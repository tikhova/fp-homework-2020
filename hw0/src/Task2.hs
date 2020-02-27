module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- | Proof of double negation. Unfolded type is:
-- a -> Neg a -> Void
-- a -> (a -> Void) -> Void
doubleNeg :: a -> Neg (Neg a)
doubleNeg a aToVoid = aToVoid a

-- | Proof of Glivenko theorem. Unfolded type is:
-- (Neg (Either a (Neg a))) -> Void
-- ((Either a (Neg a)) -> Void) -> Void
-- ((Either a (a -> Void)) -> Void) -> Void
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg eitherToVoid = let negA = eitherToVoid . Left in 
  eitherToVoid (Right negA)

-- | pierce is uninhabitable
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | doubleNegElim is uninhabitable
-- ((a -> Void) -> Void) -> a  
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | Proof of triple negation. Unfolded type is:
--  Neg (Neg (a -> Void)) -> (a -> Void)
--  Neg ((a -> Void) -> Void) -> (a -> Void)
-- (((a -> Void) -> Void) -> Void) -> a -> Void
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f = f . doubleNeg
