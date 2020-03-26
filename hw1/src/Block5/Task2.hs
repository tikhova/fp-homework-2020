module Block5.Task2 (moving) where

import           Control.Monad.State (State, evalState, get, put)
import           Data.Sequence       (Seq (..), drop)

-- | Puts average of n-sized windows in a list.
moving :: Int -> [Float] -> [Float]
moving n list = reverse $ evalState (moveWithState list) Empty
  where
    moveWithState :: [Float] -> State (Seq Float) [Float]
    moveWithState [] = return []
    moveWithState (x : xs) = do
      w <- get
      let newWindow = nextWindow n x w
      let m = mean newWindow
      put newWindow
      fmap (m :) (moveWithState xs)

    nextWindow :: Int -> a -> Seq a -> Seq a
    nextWindow l x w
      | length w < l = w :|> x
      | otherwise    = Data.Sequence.drop 1 w :|> x

    mean :: Seq Float -> Float
    mean w = sum w / fromIntegral (length w)
