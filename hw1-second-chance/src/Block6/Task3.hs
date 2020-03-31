{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block6.Task3
  ( bsParser
  , intParser
  ) where

import           Block6.Task1        (Parser (..))
import           Block6.Task2        (element, eof, ok, satisfy)
import           Control.Applicative (some, (<|>))
import           Data.Char           (isDigit)

-- | Parser of bracket sequences. Falls if the sequence is incorrect.
bsParser :: Parser Char ()
bsParser = helper *> eof
  where
    helper :: Parser Char ()
    helper = element '(' *> helper *> element ')' *> helper <|> ok

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
    numberParser = read <$> some (satisfy isDigit)
