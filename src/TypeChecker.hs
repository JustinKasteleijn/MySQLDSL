module TypeChecker where

import           AST.DBAST (Type (TInt, TText), Value (VInt, VText),
                            showEmptyVal, typeOfValue)

eqTypeAndValue :: [String] -> [Type] -> [Value] -> Either String ()
eqTypeAndValue cols types values
  | length types /= length values = Left "Number of columns and values do not match"
  | otherwise = go (zip3 [0..] cols (zip types values))
  where
    go :: [(Int, String, (Type, Value))] -> Either String ()
    go [] = Right ()
    go ((i, col, (t, v)) : xs)
      | eqTypeValue t v = go xs
      | otherwise = Left $ "Type mismatch in column '" ++ col
                           ++ "' at position " ++ show i
                           ++ ": expected " ++ show t
                           ++ ", got " ++ show (typeOfValue v)
                           ++ " (" ++ showEmptyVal v ++ ") "
    eqTypeValue :: Type -> Value -> Bool
    eqTypeValue TInt  (VInt _)  = True
    eqTypeValue TText (VText _) = True
    eqTypeValue _ _             = False


eqValueTypes :: Value -> Value -> Either String ()
eqValueTypes (VInt _) (VInt _)   = pure ()
eqValueTypes (VText _) (VText _) = pure ()
eqValueTypes v v'                = Left $ "Types "
                                      ++ show (typeOfValue v)
                                      ++ " and "
                                      ++ show (typeOfValue v')
                                      ++ "do not match"
