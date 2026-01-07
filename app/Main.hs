module Main where

import           Interpreter             (execute)
import           Parsers.StatementParser (parse, parseStatement)
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
  let input =  "SELECT name FROM Person WHERE age = Hello" -- "INSERT INTO Person ( name, age, course ) VALUES ( Saba, 26, ScaryStuff )"
      tokens = tokenize input
  case parse parseStatement tokens of
    Left err        -> putStrLn $ "Parse error: " ++ err
    Right (stmt, _) -> execute stmt >>= print
