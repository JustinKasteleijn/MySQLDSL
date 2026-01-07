{-# LANGUAGE InstanceSigs #-}

module AST.DBAST where

import           Data.List (intercalate)
import qualified Data.Map  as M

-- Database

type TableName = String

type ColumnName = String

data Type
  = TInt
  | TText

mkType :: String -> Type
mkType "INT"  = TInt
mkType "TEXT" = TText
mkType _      = error "Type does not exist"

instance Show Type where
  show :: Type -> String
  show TInt  = "INT"
  show TText = "TEXT"

data Value
  = VInt Int
  | VText String
  deriving (Show)

instance Eq Value where
  (==) :: Value -> Value -> Bool
  (VInt n) == (VInt m)    = n == m
  (VText t) == (VText t') = t == t'
  _ == _                  = error "Should be prevented by the type checker"

instance Ord Value where
  compare :: Value -> Value -> Ordering
  compare (VInt n) (VInt m)    = compare n m
  compare (VText t) (VText t') = compare t t'
  compare _ _                  = error "Should be prevented by the type checker"

showEmptyVal :: Value -> String
showEmptyVal (VInt n)  = show n
showEmptyVal (VText t) = show t

typeOfValue :: Value -> Type
typeOfValue (VInt _)  = TInt
typeOfValue (VText _) = TText

type Row = [Value]

data Table = Table
  { columns :: [ColumnName],
    rows    :: [Row]
  }
  deriving (Show)

type DB = M.Map TableName Table

-- Statements

data Statement
  = Create TableName [(ColumnName, Type)]     -- CREATE TABLE Person ( name TEXT, age INT )
  | Insert TableName [ColumnName] [Value]     -- INSERT INTO Person ( name, age) VALUES (26, "Saba")
  | Select [SelectStatement] TableName (Maybe Condition) (Maybe [ColumnName]) -- SELECT name, COUNT(age) FROM Person WHERE age > 5 GROUP BY course

instance Show Statement where
  show :: Statement -> String
  show (Create table cols)
    = "CREATE TABLE " ++ table ++ " ( " ++ showCols cols ++ ")"
    where
      showCols :: [(ColumnName, Type)] -> String
      showCols = concatMap (\(c, t) -> show c ++ " " ++ show t ++ " ")
  show (Insert table cols vals)
    = "INSERT INTO " ++ table ++ " ( " ++ intercalate ", " cols ++ " ) VALUES ( " ++ intercalate ", " (map show vals) ++ " )"
  show (Select cols name cond group)
    = "SELECT "                                         -- SELECT
      ++ intercalate ", " (map show cols)               -- course, COUNT (name)
      ++ "\nFROM "                                      -- FROM
      ++ name                                           -- Person
      ++ "\n"                                           --
      ++ maybe "" (\c -> "WHERE " ++ show c) cond       -- WHERE age > 25
      ++ maybe "" (intercalate ", ") group              -- GROUP BY course

data Condition
  = Eq ColumnName Value             -- name = "Justin"
  | LessThan ColumnName Value       -- age < 25
  | GreaterThan ColumnName Value    -- age > 25
  | And Condition Condition         -- age > 25 AND age == 25
  | Or Condition Condition          -- age > 25 OR age == 25
  | Not Condition                   -- NOT age > 25

instance Show Condition where
  show :: Condition -> String
  show (Eq col val)          = col ++ " = " ++ showEmptyVal val
  show (LessThan col val)    = col ++ " < " ++ showEmptyVal val
  show (GreaterThan col val) = col ++ " > " ++ showEmptyVal val
  show (And cond cond')      = show cond ++ " AND " ++ show cond'
  show (Or cond cond')       = show cond ++ " OR " ++ show cond'
  show (Not cond)            = "NOT ( " ++ show cond ++ " )"

data SelectStatement
  = Column ColumnName
  | Aggregate AggregateFunc ColumnName

instance Show SelectStatement where
  show :: SelectStatement -> String
  show (Column col)        = show col
  show (Aggregate fun col) = show fun ++ "( " ++ show col ++ " )"

data AggregateFunc
  = SUM
  | COUNT
  deriving (Show)
