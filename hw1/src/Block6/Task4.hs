{-# LANGUAGE InstanceSigs #-}

module Block6.Task4 (listlistParser) where

import           Block6.Task1        (Parser (..))
import           Block6.Task2        (element, satisfy)
import           Block6.Task3        (collectParser, intParser)
import           Control.Applicative (empty)
import           Data.Maybe          (fromJust)

-- | Parser that consumes the whole input and returns it with all spaces removed.
removeSpacesParser :: Parser Char String
removeSpacesParser = collectParser $ satisfy (/= ' ')

-- | Returns parser of int list of given size
listParser 
        :: Int               -- size of the list
        -> Parser Char [Int] -- list parser
listParser n
  | n == 0    = pure []
  | n > 0     = (:) <$> (element ',' *> intParser) <*> listParser (n - 1)
  | otherwise = empty

-- | Returns parser of list of int lists. Before each list in the string comes its size.
listlistParser :: Parser Char [[Int]]
listlistParser = Parser $ \s ->
  runParser
    (collectParser $ intParser >>= listParser)
    (fst $ fromJust $ runParser removeSpacesParser s)
