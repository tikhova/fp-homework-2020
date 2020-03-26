module Block4.Task1 (stringSum) where

import           Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum str = fmap sum numList
  where
    numList = traverse readMaybe strList
    strList = words str
