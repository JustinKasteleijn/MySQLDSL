module StringUtils where

escape :: String -> String
escape = concatMap esc
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc c    = [c]

quote :: String -> String
quote s = "\"" ++ escape s ++ "\""
