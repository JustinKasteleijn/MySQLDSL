module Interpreter
  (
    execute
  )
where

import           AST.DBAST         (ColumnName, Condition (..), Row,
                                    Statement (..), Table (Table), TableName,
                                    Type, Value, mkType, showEmptyVal)
import           Control.Monad     (foldM)
import           Data.List         (elemIndex, foldl', groupBy, intercalate,
                                    sortOn)
import           Parsers.CSVParser (csv)
import           Parsers.Parser    (parse)
import           System.Directory  (doesFileExist)
import           TypeChecker

data InterpreterResult
  = Ok
  | OkTable Table
  deriving (Show)

buildPath :: String -> String
buildPath = ("tables/" ++)

filename :: String -> String
filename name = buildPath (name ++ ".csv")

metaFilename :: String -> String
metaFilename name = buildPath (name ++ "Meta.csv")

execute :: [Statement] -> IO (Either String [InterpreterResult])
execute = foldM aux (Right [])
  where
    aux :: Either String [InterpreterResult] -> Statement -> IO (Either String [InterpreterResult])
    aux (Left err) _     = return $ Left err
    aux (Right acc) stmt = do
      res <- execute' stmt
      case res of
         Left err   -> return $ Left err
         Right res' -> return $ Right (res' : acc)

execute' :: Statement -> IO (Either String InterpreterResult)
execute' stmt =
  case stmt of
    Create name columns ->
      execCreate name columns

    Insert name cols vals ->
      execInsert name cols vals

    Select cols name cond group ->
      execSelect cols name cond group

--------------------------------------------------------------------------------
-- CREATE
--------------------------------------------------------------------------------

execCreate
  :: TableName
  -> [(ColumnName, Type)]
  -> IO (Either String InterpreterResult)
execCreate name columns = do
  let path = filename name
  exists <- doesFileExist path
  if exists
    then pure $ Left $ "Table '" ++ name ++ "' already exists"
    else do
      writeFile path (header columns)
      writeFile (metaFilename name) (meta columns ++ "\n")
      pure $ Right Ok
  where
    header = (++ "\n") . intercalate "," . map fst
    meta   = intercalate "," . map (show . snd)

--------------------------------------------------------------------------------
-- INSERT
--------------------------------------------------------------------------------

execInsert
  :: TableName
  -> [ColumnName]
  -> [Value]
  -> IO (Either String InterpreterResult)
execInsert name cols vals = do
  exists <- doesFileExist (filename name)
  if not exists
    then pure $ Left $ "Table '" ++ name ++ "' does not exist"
    else do
      meta <- readFile (metaFilename name)
      case parse csv meta of
        Left err -> pure $ Left err
        Right ((types, _), _) -> do
          let types' = map mkType types
          case eqTypeAndValue cols types' vals of
            Left err -> pure $ Left err
            Right () -> do
              appendFile (filename name) (row vals)
              pure $ Right Ok
  where
    row = (++ "\n") . intercalate "," . map showEmptyVal

--------------------------------------------------------------------------------
-- SELECT
--------------------------------------------------------------------------------

filterRows :: Maybe Condition -> [ColumnName] -> [Row] -> Either String [Row]
filterRows Nothing _ rows = Right rows
filterRows (Just cond) header rows =
    traverse (\r -> fmap (\b -> if b then r else []) (evalCondition cond header r)) rows
  >>= \rs -> Right $ filter (not . null) rs

groupRows :: [Int] -> [Row] -> [Row]
groupRows gIndices rows =
    let sorted  = sortOn (\r -> map (r !!) gIndices) rows
        grouped = map head $ groupBy (\a b -> map (a !!) gIndices == map (b !!) gIndices) sorted
    in grouped

projectRows :: [Int] -> [Row] -> [Row]
projectRows indices = map (project indices)

execSelect
  :: [ColumnName]           -- SELECT columns
  -> TableName              -- FROM table
  -> Maybe Condition        -- optional WHERE
  -> Maybe [ColumnName]     -- optional GROUP BY
  -> IO (Either String InterpreterResult)
execSelect selectCols tableName cond mGroup = do
    -- Check table
    exists <- doesFileExist (filename tableName)
    if not exists
      then return $ Left $ "Table '" ++ tableName ++ "' does not exist"
      else do
        file <- readFile (filename tableName)

        -- Parse csv
        let parsed = parse csv file

        -- run all select steps
        return $ do
          ((header, rows), _) <- parsed

          -- Filter rows
          filteredRows <- filterRows cond header rows

          -- Apply GROUP BY if present
          groupedRows <- case mGroup of
            Nothing -> Right filteredRows
            Just groupCols -> do
              gIndices <- columnIndices header groupCols
              Right $ groupRows gIndices filteredRows

          colIndices <- columnIndices header selectCols
          let finalRows = projectRows colIndices groupedRows

          return $ OkTable $ Table selectCols finalRows



columnIndices :: [ColumnName] -> [ColumnName] -> Either String [Int]
columnIndices header =
  traverse lookupCol
  where
    lookupCol col =
      case elemIndex col header of
        Nothing -> Left $ "Unknown column '" ++ col ++ "'"
        Just i  -> Right i

project :: [Int] -> Row -> Row
project is row = map (row !!) is

evalCondition :: Condition -> [ColumnName] -> Row -> Either String Bool
evalCondition (Eq col val) header row = do
  val' <- getColFromRow col header row
  _    <- eqValueTypes val' val
  return $ val' == val
evalCondition (GreaterThan col val) header row = do
  val' <- getColFromRow col header row
  _    <- eqValueTypes val' val
  return $ val' > val
evalCondition (LessThan col val) header row = do
  val' <- getColFromRow col header row
  _    <- eqValueTypes val' val
  return $ val' < val
evalCondition (And cond cond') header row = do
  res  <- evalCondition cond header row
  res' <- evalCondition cond' header row
  return $ res' && res
evalCondition (Or cond cond') header row = do
  res  <- evalCondition cond header row
  res' <- evalCondition cond' header row
  return $ res' || res
evalCondition (Not cond) header row = do
  res <- evalCondition cond header row
  return $ not res

getColFromRow :: ColumnName -> [ColumnName] -> Row -> Either String Value
getColFromRow col header row
  = case elemIndex col header of
      Nothing -> Left $ "Column: " ++ col ++ " does not exist"
      Just i  -> Right $ row !! i
