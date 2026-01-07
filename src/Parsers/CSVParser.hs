module Parsers.CSVParser (csv) where

import           AST.CSVAST          (CSV)
import           AST.DBAST           (Value (VInt, VText))
import           Control.Applicative (many, some, (<|>))
import           Data.Char           (isAlphaNum)
import           Parsers.Parser      (Parser (..), char, digits1, lexeme,
                                      satisfy, sepBy0, sepBy1)

quotedText :: Parser String String
quotedText = do
    _ <- char '"'
    content <- many (satisfy (/= '"'))
    _ <- char '"'
    return content

unquotedText :: Parser String String
unquotedText = some (satisfy isAlphaNum)

value :: Parser String Value
value = parseInt <|> parseText
  where
    parseInt :: Parser String Value
    parseInt = VInt . read <$> digits1

    parseText :: Parser String Value
    parseText = VText <$> (quotedText <|> unquotedText)


csv :: Parser String CSV
csv = do
    header <- sepBy1 (char ',') (some (satisfy isAlphaNum))
    _ <- char '\n'
    values <- sepBy0 (char '\n') row
    return (header, values)
  where
    row :: Parser String [Value]
    row = sepBy1 (lexeme (char ',')) value
