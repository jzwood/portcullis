module Test where

import Syntax
import Transpile
import Typecheck
import qualified Data.Map as Map

--x = sequence [typeofExpr m s (Val $ Number 0), typeofExpr m s (Ident "x")]

{-
exprType = x
  >>= \[t1, t2] ->
    typecheckExpr t1 (typeofBop Minus)
      >>= typecheckExpr t2

te = typecheckExpr (Arrow NumType NumType) NumType
-}

 -- ====================

stmt = Function { name = "tail"
                , signature = Arrow (ListType (Unspecfied "x")) (ListType (Unspecfied "x"))
                , args = ["xs"]
                , body = TernOp Slice (Ident "xs") (Val $ Number 1) (UnOp Length (Ident "xs"))
                }
m = Map.singleton "tail" stmt
b = typeofExpr m stmt $ Ident "xs"
c = (typeofExpr m stmt) (UnOp Length (Ident "xs"))

tce = typecheckExpr (ListType (Unspecfied "x")) (typeofUnOp Length)
--tc = Typecheck.typecheck (ListType (Unspecfied "x")) (typeofUnOp Length) Map.empty  -- compare type with sig


huhu = Typecheck.typecheck (ListType $ Unspecfied "a") (ListType $ Unspecfied "b") Map.empty

main :: IO ()
main = do
  --print $ xx
  --print b
  --print c
  --print tce
  print huhu
