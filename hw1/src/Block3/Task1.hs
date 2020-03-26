module Block3.Task1 (maybeConcat) where

import           Data.Maybe (fromMaybe)

-- | Get list of Maybe lists and return concatenation of Just elements.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat x = fromMaybe [] (mconcat x)
