module Tokenizer where

import           Data.Char (isAlphaNum, isSpace)

data Token
  = TCreate
  | TTable
  | TInsert
  | TInto
  | TValues
  | TSELECT
  | TFROM
  | TWHERE
  | TIdent String
  | TType String
  | TLParen
  | TRParen
  | TNewline
  | TComma
  | TEq
  | TGt
  | TLt
  | TNot
  | TAnd
  | TOr
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | isAlphaNum c =
      let (word, rest) = span isAlphaNum (c : cs)
       in classifyKeyword word : tokenize rest
  | c == '('  = TLParen : tokenize cs
  | c == ')'  = TRParen : tokenize cs
  | c == ','  = TComma : tokenize cs
  | c == '='  = TEq : tokenize cs
  | c == '>'  = TGt : tokenize cs
  | c == '<'  = TLt : tokenize cs
  | c == '\n' = TNewline : tokenize cs
  | isSpace c = tokenize cs
  | otherwise = error $ "Unexpected character: " ++ [c]

classifyKeyword :: String -> Token
classifyKeyword "CREATE" = TCreate
classifyKeyword "TABLE"  = TTable
classifyKeyword "INSERT" = TInsert
classifyKeyword "INTO"   = TInto
classifyKeyword "VALUES" = TValues
classifyKeyword "SELECT" = TSELECT
classifyKeyword "FROM"   = TFROM
classifyKeyword "WHERE"  = TWHERE
classifyKeyword "INT"    = TType "INT"
classifyKeyword "TEXT"   = TType "TEXT"
classifyKeyword "NOT"    = TNot
classifyKeyword "AND"    = TAnd
classifyKeyword "OR"     = TOr
classifyKeyword w        = TIdent w
