{-# LANGUAGE LambdaCase #-}

module Block6.Task2
  ( element
  , eof
  , ok
  , satisfy
  , stream
  ) where

import           Block6.Task1 (Parser (..))

-- | Parser that never falls or consumes input.
ok :: Parser s ()
ok = pure ()

-- | Parser that checks there is no more input or falls.
eof :: Parser s ()
eof = Parser $ \case
  [] -> Just ((), [])
  _  -> Nothing

-- | Parser that gets predicate and returns element (consuming it from the input) if the predicate holds, otherwise falls.
satisfy :: (s -> Bool) -> Parser s s
satisfy cond = Parser $ \case
  []       -> Nothing
  (x : xs) ->
    if cond x
    then Just (x, xs)
    else Nothing

-- | Parser that parses single element.
element :: Eq s => s -> Parser s s
element x = satisfy (== x)

-- | Parser that parses several elements.
stream :: Eq s => [s] -> Parser s [s]
stream xs = Parser $ \s ->
  case remainsAfter xs s of
    Nothing -> Nothing
    Just s' -> Just (xs, s')
  where
    remainsAfter :: Eq s => [s] -> [s] -> Maybe [s]
    remainsAfter [] t          = Just t
    remainsAfter (y:ys) (z:zs) =
      if y == z
      then remainsAfter ys zs
      else Nothing
    remainsAfter _  _          = Nothing
