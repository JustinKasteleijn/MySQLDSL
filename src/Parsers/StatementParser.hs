module Parsers.StatementParser
  (parse,
   parseStatement,
   parseStatements
  )
where


import           AST.DBAST           (ColumnName,
                                      Condition (And, Eq, GreaterThan, LessThan, Not, Or),
                                      Statement (Create, Insert, Select),
                                      Type (..), Value (VInt, VText))
import           Control.Applicative (optional, (<|>))
import           Data.Char           (isDigit)
import           Parsers.Parser
import           Tokenizer

token :: Token -> Parser [Token] Token
token = elem'

--tokens :: [Token] -> Parser [Token] [Token]
--tokens = elems

identifier :: Parser [Token] String
identifier = do
  t <- item
  case t of
    TIdent s -> pure s
    _        -> fail "Expected identifier"

type' :: Parser [Token] Type
type' = do
  t <- item
  case t of
    TType "INT"  -> pure TInt
    TType "TEXT" -> pure TText
    _            -> fail $ "Invalid type: " ++ show t

value :: Parser [Token] Value
value = parseInt <|>
  parseText
  where
    parseInt :: Parser [Token] Value
    parseInt = do
      d <- identifier
      if all isDigit d
         then return $ VInt $ read d
         else fail $ d ++ "is not of type INT"

    parseText :: Parser [Token] Value
    parseText = VText <$> identifier

parseStatements :: Parser [Token] [Statement]
parseStatements = sepBy0 (token TNewline) parseStatement

parseStatement :: Parser [Token] Statement
parseStatement = parseCreate
  <|> parseInsert
  <|> parseSelect

parseCreate :: Parser [Token] Statement
parseCreate = do
  _    <- token TCreate
  _    <- token TTable
  name <- identifier
  _    <- token TLParen
  cols <- sepBy1 (token TComma) column
  _    <- token TRParen
  return $ Create name cols
  where
    column :: Parser [Token] (ColumnName, Type)
    column = do
      col  <- identifier
      t    <- type'
      return (col, t)

parseInsert :: Parser [Token] Statement
parseInsert = do
  _    <- token TInsert
  _    <- token TInto
  name <- identifier
  _    <- token TLParen
  cols <- sepBy1 (token TComma) identifier
  _    <- token TRParen
  _    <- token TValues
  _    <- token TLParen
  vals <- sepBy1 (token TComma) value
  _    <- token TRParen
  return $ Insert name cols vals

parseSelect :: Parser [Token] Statement
parseSelect = do
  _    <- token TSELECT
  cols <- sepBy1 (token TComma) identifier
  _    <- token TFROM
  name <- identifier
  Select cols name <$> parseOptionalCondition

parseOptionalCondition :: Parser [Token] (Maybe Condition)
parseOptionalCondition = optional (token TWHERE *> parseCondition)

parseCondition :: Parser [Token] Condition
parseCondition = parseOr

parseOr :: Parser [Token] Condition
parseOr = chainl1 parseAnd (token TOr >> return Or)

parseAnd :: Parser [Token] Condition
parseAnd = chainl1 parseNot (token TAnd >> return And)

parseNot :: Parser [Token] Condition
parseNot = (token TOr >> (Not <$> parseNot))
       <|> parseAtomic

parseAtomic :: Parser [Token] Condition
parseAtomic =
      parseCompareOperant TEq Eq
  <|> parseCompareOperant TLt LessThan
  <|> parseCompareOperant TGt GreaterThan
  <|> token TLParen *> parseCondition <* token TRParen

parseCompareOperant :: Token -> (ColumnName -> Value -> Condition) -> Parser [Token] Condition
parseCompareOperant t constructor = do
  col <- identifier
  _   <- token t
  constructor col <$> value
