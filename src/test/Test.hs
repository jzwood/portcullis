module Test where

import Syntax
import Transpile
import Typecheck
import qualified Data.Map as Map

s = Function { name = "neg"
             , signature = Arrow NumType NumType
             , args = ["x"]
             , body = BinOp Minus (Val $ Number 0) (Ident "x")
             }
m = Map.singleton "neg" s

x = sequence [typeofExpr m s (Val $ Number 0), typeofExpr m s (Ident "x")]

main :: IO ()
main = do
  --print $ typeofExpr m stmt (body stmt)
--  print $ typeofExpr m s (Val $ Number 0)
--  print $ typeofExpr m s (Ident "x")
  print $ body s
  print x
--  print $ x
---    >>= \[t1, t2] -> typecheckExpr t1 (typeofBop Minus)
--    >>= typecheckExpr t1


