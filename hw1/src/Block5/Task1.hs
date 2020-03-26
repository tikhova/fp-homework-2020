module Block5.Task1
  ( Expr (..)
  , ArithmeticError (..)
  , eval
  ) where


import           Data.Maybe (fromJust)

data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving Show

data ArithmeticError = DivisionByZero | NegativePower deriving (Show, Eq)

performSafeOperation :: (Int -> Int -> Int) -> Expr -> Expr -> Either ArithmeticError Int
performSafeOperation = performOperation (const True) Nothing

performOperation :: (Int -> Bool) -> Maybe ArithmeticError -> (Int -> Int -> Int) -> Expr -> Expr -> Either ArithmeticError Int
performOperation p err f x y = fmap f (eval x) <*> evalWithPredicate y p err

evalWithPredicate :: Expr -> (Int -> Bool) -> Maybe ArithmeticError -> Either ArithmeticError Int
evalWithPredicate expr p err =
  case eval expr of
    Left e    -> Left e
    Right val ->
      if p val
      then Right val
      else Left $ fromJust err

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Add x y) = performSafeOperation (+) x y
eval (Sub x y) = performSafeOperation (-) x y
eval (Mul x y) = performSafeOperation (*) x y
eval (Div x y) = performOperation (/= 0) (Just DivisionByZero) div x y
eval (Pow x y) = performOperation (>= 0) (Just NegativePower) (^) x y
