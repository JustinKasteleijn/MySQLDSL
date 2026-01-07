module Main where

import           Debug.Trace             (trace)
import           Interpreter             (execute)
import           Parsers.StatementParser (parse, parseStatements)
import           Tokenizer

-- main :: IO ()
-- main = do
--   let input = "CREATE TABLE Person ( name TEXT, age INT )"
--       tokens = tokenize input
--   case parse parseStatement tokens of
--     Left err        -> putStrLn $ "Parse error: " ++ err
--     Right (stmt, _) -> execute stmt >>= print

main :: IO ()
main = do
  let input = "SELECT name FROM Person"
      tokens = tokenize input
      tokens' = trace (show tokens) tokens
  case parse parseStatements tokens' of
    Left err        -> putStrLn $ "Parse error: " ++ err
    Right (stmt, _) -> execute stmt >>= print
