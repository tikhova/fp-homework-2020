module Block4.Task1 (stringSum) where

import           Text.Read (readMaybe)

-- | Get string of numbers separated by spaces. Return the sum of the numbers.
-- If any of the string elements cannot be converted into number, return Nothing.
stringSum :: String -> Maybe Int
stringSum str = fmap sum numList
  where
    numList = traverse readMaybe strList
    strList = words str
