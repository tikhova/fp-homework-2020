module Task6
  ( expr1
  , expr2
  ) where

-- | WHNF of
-- > f = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
expr1 :: (Either String b, Either String c)
expr1 = (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | WHNF of
-- > f = null $ mapMaybe foo "pole chudes ochen' chudesno"
-- where
--
-- @
--     foo :: Char -> Maybe Double
--       foo char =
--         if char == 'o' 
--         then Just $ exp pi
--         else Nothing
-- @
expr2 :: Bool
expr2 = False
