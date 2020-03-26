{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block6.Task3
  ( bsParser
  , collectParser
  , intParser
  ) where

import           Block6.Task1        (Parser (..), first)
import           Block6.Task2        (element, eof, ok, satisfy)
import           Control.Applicative (empty, (<|>))
import           Data.Char           (isDigit)

-- | Parser of bracket sequences. Falls if the sequence is incorrect.
bsParser :: Parser Char ()
bsParser = balanceParser <* eof >>= checkBalance
  where
    balanceParser :: Parser Char Int
    balanceParser = Parser balanceParserF
      where
        balanceParserF :: String -> Maybe (Int, String)
        balanceParserF s =
          case runParser bracketParser s of
            Nothing      -> Just (0, s)
            Just (x, s') -> first (changeBalance x) <$> runParser balanceParser s'

    bracketParser :: Parser Char Char
    bracketParser = element '(' <|> element ')'

    changeBalance :: Char -> Int -> Int
    changeBalance '(' = (+) 1
    changeBalance ')' = (-) 1
    changeBalance _   = id

    checkBalance :: Int -> Parser Char ()
    checkBalance x
      | x == 0    = ok
      | otherwise = empty

-- | Parser that parses integers.
intParser :: Parser Char Int
intParser = (setSign <$> signParser <*> numberParser) <|> numberParser
  where
    setSign :: Char -> Int -> Int
    setSign '-' = negate
    setSign _   = id

    signParser :: Parser Char Char
    signParser = element '+' <|> element '-'

    numberParser :: Parser Char Int
    numberParser = read <$> collectParser digitParser
      where
        digitParser :: Parser Char Char
        digitParser = satisfy isDigit

-- | Collects results of the given parser until the end of input.
-- Skips Nothings.
collectParser :: forall s a . Parser s a -> Parser s [a]
collectParser (Parser f) = Parser collectF
  where
    collectF :: [s] -> Maybe ([a], [s])
    collectF s =
      case f s of
        Just (x, s') -> first (x :) <$> collectF s'
        Nothing      -> Just ([], s)


