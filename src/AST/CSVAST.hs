module AST.CSVAST where

import           AST.DBAST (ColumnName, Row)

type CSV = ([ColumnName], [Row])
