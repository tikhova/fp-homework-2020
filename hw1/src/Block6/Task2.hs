{-# LANGUAGE LambdaCase #-}

module Block6.Task2
  ( element
  , eof
  , ok
  , satisfy
  , stream
  ) where

import           Block6.Task1 (Parser (..))

ok :: Parser s ()
ok = pure ()

eof :: Parser s ()
eof = Parser $ \case
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy cond = Parser $ \case
  []       -> Nothing
  (x : xs) ->
    if cond x
    then Just (x, xs)
    else Nothing

element :: Eq s => s -> Parser s s
element x = satisfy (== x)

stream :: Eq s => [s] -> Parser s [s]
stream xs = Parser $ \s ->
  case remainsAfter xs s of
    Nothing -> Nothing
    Just s' -> Just (xs, s')
  where
    remainsAfter :: [s] -> [s] -> Maybe [s]
    remainsAfter [] t          = Just t
    remainsAfter (_:ys) (_:zs) = remainsAfter ys zs
    remainsAfter _  _          = Nothing
