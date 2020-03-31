{-# LANGUAGE InstanceSigs #-}

module Block6.Task4 (listlistParser) where

import           Block6.Task1        (Parser (..))
import           Block6.Task2        (element, eof)
import           Block6.Task3        (intParser)
import           Control.Applicative (empty, many, (<|>))

-- | Parser that consumes the whole input and returns it with all spaces removed.
skipSpacesParser :: Parser Char String
skipSpacesParser = many $ element ' '

-- | Returns parser of int list of given size
listParser
        :: Int               -- size of the list
        -> Parser Char [Int] -- list parser
listParser n
  | n == 0    = pure []
  | n > 0     = (:) <$>
  (skipSpacesParser *> element ',' *> skipSpacesParser *> intParser) <*> listParser (n - 1)
  | otherwise = empty

-- | Returns parser of int list of given its string that states size
listWithSizeParser :: Parser Char [Int]
listWithSizeParser = skipSpacesParser *> (intParser >>= listParser) <* skipSpacesParser

-- | Returns parser of list of int lists. Before each list in the string comes its size.
listlistParser :: Parser Char [[Int]]
listlistParser = many (listWithSizeParser <* element ',' <|> listWithSizeParser <* eof) <* eof
