{-# LANGUAGE NoMonomorphismRestriction #-}

module Task7
  ( expr1
  , expr2
  , expr3
  ) where

import Data.Either (lefts, rights)
      
-- | Deriving types of all subterms of:
-- > null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
expr1 :: Bool
expr1 = t16 `t17` t12
  where
    t1  = "Dorian " :: String
    t2  = " Grey" :: String
    t3  = (++) :: String -> String -> String
    t4  = t3 t1 :: String -> String
    t5  = (t4, t2) :: (String -> String, String)
    t6  = [t5] :: [(String -> String, String)]
    t7  = uncurry :: ((String -> String) -> String -> String) -> (String -> String, String) -> String
    t8  = id :: (String -> String) -> String -> String
    t9  = t7 t8 :: (String -> String, String) -> String
    t10 = map :: ((String -> String, String) -> String) -> [(String -> String, String)] -> [String]
    t11 = t10 t9 :: [(String -> String, String)] -> [String]
    t12 = t11 t6 :: [String]
    t13 = null :: String -> Bool
    t14 = (.) :: (String -> Bool) -> ([String] -> String) -> [String] -> Bool
    t15 = head :: [String] -> String
    t16 = t13 `t14` t15 :: [String] -> Bool
    t17 = ($) :: ([String] -> Bool) -> [String] -> Bool

-- | Deriving types of all subterms of:
-- > (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
expr2 :: (Num t, Num a) => [(t, a)]
expr2 = t18
  where
    t1  = 2 :: Num a => a
    t2  = 6 :: Integral b => b
    t3  = (^) :: (Integral b, Num a) => a -> b -> a
    t4  = (t1 `t3` t2) :: Num a => a
    t5  = 1 :: Num t => t
    t6  = 2 :: Num t => t
    t7  = (+) :: Num t => t -> t -> t
    t8  = t5 `t7` t6 :: Num t => t
    t9  = Right :: a -> Either t a
    t10 = t9 t4 :: Num a => Either t a
    t11 = Left :: t -> Either t a
    t12 = t11 t8 :: Num t => Either t a
    t13 = [t10, t12] :: (Num t, Num a) => [Either t a]
    t14 = zip :: [t] -> [a] -> [(t, a)]
    t15 = lefts :: [Either t a] -> [t]
    t16 = rights :: [Either t a] -> [a]
    t17 = (\x -> t14 (t15 x) (t16 x)) :: [Either t a] -> [(t, a)]
    t18 = t17 t13 :: (Num t, Num a) => [(t, a)]

-- | Deriving types of all subterms of:
--
-- @
--      let impl = \x y -> not x || y in
--        let isMod2 = \x -> x `mod` 2 == 0 in
--        let isMod4 = \x -> x `mod` 4 == 0 in
--        \x -> (isMod4 x) `impl` (isMod2 x)
-- @
expr3 :: (Integral a, Eq a) => a -> Bool
expr3 = \x -> isMod4 x `impl` isMod2 x
  where
    t1     = not :: Bool -> Bool
    t2     = (||) :: Bool -> Bool -> Bool
    impl   = (\x y -> t1 x `t2` y) :: Bool -> Bool -> Bool
    t3     = mod :: Integral a => a -> a -> a
    t4     = (==) :: Eq a => a -> a -> Bool
    t5     = 2 :: (Num a, Integral a) => a
    t6     = 4 :: (Num a, Integral a) => a
    t7     = 0 :: (Num a, Eq a) => a
    isMod2 = (\x -> x `t3` t5 `t4` t7) :: (Integral a, Eq a) => a -> Bool
    isMod4 = (\x -> x `t3` t6 `t4` t7) :: (Integral a, Eq a) => a -> Bool
        