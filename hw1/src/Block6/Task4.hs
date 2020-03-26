{-# LANGUAGE InstanceSigs #-}

module Block6.Task4 (listlistParser) where

import           Block6.Task1        (Parser (..))
import           Block6.Task2        (element, satisfy)
import           Block6.Task3        (collectParser, intParser)
import           Control.Applicative (empty)
import           Data.Maybe          (fromJust)

removeSpacesParser :: Parser Char String
removeSpacesParser = collectParser $ satisfy (/= ' ')

listParser :: Int -> Parser Char [Int]
listParser n
  | n == 0    = pure []
  | n > 0     = (:) <$> (element ',' *> intParser) <*> listParser (n - 1)
  | otherwise = empty

listlistParser :: Parser Char [[Int]]
listlistParser = Parser $ \s ->
  runParser 
    (collectParser $ intParser >>= listParser)
    (fst $ fromJust $ runParser removeSpacesParser s)
