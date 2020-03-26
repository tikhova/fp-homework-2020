module Block3.Task1 (maybeConcat) where

import           Data.Maybe (catMaybes)

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = concat . catMaybes
