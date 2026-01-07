module Parsers.JSONParser where

import           AST.JSONAST
import           Control.Applicative (many, (<|>))
import           Parsers.Parser

parseJson :: Parser String JSON
parseJson = lexeme jnull
    <|> lexeme jbool
    <|> lexeme jnumber
    <|> lexeme jstring
    <|> lexeme jarray
    <|> lexeme jobject

jnull :: Parser String JSON
jnull = JNull <$ string "null"

jbool :: Parser String JSON
jbool = JBool <$> (true <|> false)
  where
    true :: Parser String Bool
    true = True <$ string "true"

    false :: Parser String Bool
    false = False <$ string "false"

jnumber :: Parser String JSON
jnumber = JNumber <$> int

jstring :: Parser String JSON
jstring = JString <$> (char '"' *> many (satisfy (\c -> c /= '"' && c /= '\\')) <* char '"')

jarray :: Parser String JSON
jarray = JArray <$> (lexeme (char '[') *> sepBy0 (lexeme (char ',')) parseJson <* lexeme (char ']'))

jobject :: Parser String JSON
jobject = JObject <$> (lexeme (char '{') *> sepBy0 (lexeme (char ',')) pair <* lexeme (char '}'))
  where
    pair :: Parser String (String, JSON)
    pair = do
      JString key <- lexeme jstring
      _ <- lexeme (char ':')
      value <- parseJson
      return (key, value)
