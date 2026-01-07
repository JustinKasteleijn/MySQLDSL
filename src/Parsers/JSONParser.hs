module Parsers.JSONParser where

import           AST.JSONAST
import           Control.Applicative (many, (<|>))
import           Data.Functor        (($>))
import           Parsers.Parser

parseJson :: Parser String JSON
parseJson = lexeme jnull
    <|> lexeme jbool
    <|> lexeme jnumber
    <|> lexeme jstring
    <|> lexeme jarray
    <|> lexeme jobject

jnull :: Parser String JSON
jnull = JNull <$ elems "null"

jbool :: Parser String JSON
jbool = JBool <$> (true <|> false)
  where
    true :: Parser String Bool
    true = True <$ elems "true"

    false :: Parser String Bool
    false = False <$ elems "false"

jnumber :: Parser String JSON
jnumber = JNumber <$> int

jstring :: Parser String JSON
jstring = JString <$> (elem' '"' *> many (satisfy (\c -> c /= '"' && c /= '\\')) <* elem' '"')

jarray :: Parser String JSON
jarray = JArray <$> (lexeme (elem' '[') *> sepBy0 (lexeme (elem' ',')) parseJson <* lexeme (elem' ']'))

jobject :: Parser String JSON
jobject = JObject <$> (lexeme (elem' '{') *> sepBy0 (lexeme (elem' ',')) pair <* lexeme (elem' '}'))
  where
    pair :: Parser String (String, JSON)
    pair = do
      JString key <- lexeme jstring
      _ <- lexeme (elem' ':')
      value <- parseJson
      return (key, value)

ws :: Parser String ()
ws = many (satisfy (`elem` " \n\t")) $> ()

lexeme :: Parser String a -> Parser String a
lexeme p = ws *> p <* ws
