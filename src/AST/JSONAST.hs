{-# LANGUAGE InstanceSigs #-}

module AST.JSONAST where

import           Data.Char   (toLower)
import           Data.List   (intercalate)
import           StringUtils (quote)

data JSON
  = JObject [(String, JSON)]
  | JArray  [JSON]
  | JString String
  | JNumber Int
  | JBool   Bool
  | JNull
  deriving (Eq)

instance Show JSON where
  show :: JSON -> String
  show JNull       = "null"
  show (JBool b)   = map toLower $ show b
  show (JNumber n) = show n
  show (JString s) = quote s
  show (JArray xs) = "[ " ++ intercalate ", " (map show xs)  ++ " ]"
  show (JObject o) = "{ " ++ intercalate ", " (pairs o) ++ " }"
    where
      pairs :: [(String, JSON)] -> [String]
      pairs = map (\(s, ob) -> quote s ++ ":" ++ show ob)
