module Parsers.CSVParser (csv) where

import           AST.CSVAST          (CSV)
import           AST.DBAST           (Value (VInt, VText))
import           Control.Applicative (many, some, (<|>))
import           Data.Char           (isAlphaNum)
import           Parsers.JSONParser  (ws)
import           Parsers.Parser      (Parser (..), digits1, elem', satisfy,
                                      sepBy0, sepBy1)

quotedText :: Parser String String
quotedText = do
    _ <- elem' '"'
    content <- many (satisfy (/= '"'))
    _ <- elem' '"'
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
    header <- sepBy1 (elem' ',') (some (satisfy isAlphaNum))
    _ <- elem' '\n'
    values <- sepBy0 (elem' '\n') row
    return (header, values)
  where
    row :: Parser String [Value]
    row = sepBy1 (ws *> elem' ',' <* ws) value
