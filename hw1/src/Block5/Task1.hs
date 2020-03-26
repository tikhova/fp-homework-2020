module Block5.Task1
  ( Expr (..)
  , ArithmeticError (..)
  , eval
  ) where

import           Data.Maybe (fromJust)

-- | Arithmetical expression representation.
data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving Show

-- | Arithmetical error representation.
data ArithmeticError = DivisionByZero | NegativePower deriving (Show, Eq)

-- | Reduces two expressions to their int values and performs safe operation on them.
performSafeOperation 
                  :: (Int -> Int -> Int)        -- operation on integers
                  -> Expr                       -- first expression
                  -> Expr                       -- second expression
                  -> Either ArithmeticError Int -- result of operation
performSafeOperation = performOperation (const True) Nothing

-- | Reduces two expressions to their int values and performs operation on them.
-- If the condition on second value doesn't hold, returns error,
-- otherwise returns result of the operation.
performOperation 
              :: (Int -> Bool)              -- condition on second value
              -> Maybe ArithmeticError      -- possible error
              -> (Int -> Int -> Int)        -- operation on integers
              -> Expr                       -- first expression 
              -> Expr                       -- second expression
              -> Either ArithmeticError Int -- result of operation
performOperation p err f x y = fmap f (eval x) <*> evalWithCondition y p err

-- | Reduces expression to its int value, if condition holds returns value,
-- otherwise returns given error. If posible error is Nothing condition should be const True!
evalWithCondition 
               :: Expr                       -- arithmetic expression
               -> (Int -> Bool)              -- condition
               -> Maybe ArithmeticError      -- possible error
               -> Either ArithmeticError Int -- result
evalWithCondition expr p err =
  case eval expr of
    Left e    -> Left e
    Right val ->
      if p val
      then Right val
      else Left $ fromJust err

-- | Reduces expression to its int value, if any errors occurs returns it,
-- otherwise returns int value.
eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Add x y) = performSafeOperation (+) x y
eval (Sub x y) = performSafeOperation (-) x y
eval (Mul x y) = performSafeOperation (*) x y
eval (Div x y) = performOperation (/= 0) (Just DivisionByZero) div x y
eval (Pow x y) = performOperation (>= 0) (Just NegativePower) (^) x y
