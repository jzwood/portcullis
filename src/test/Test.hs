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

stmt = Function { name = "len"
                , signature = Arrow (ListType (Unspecfied "x")) NumType
                , args = ["xs"]
                , body = UnOp Length (Ident "xs")
                }
m = Map.singleton "len" stmt
toe = typeofExpr m stmt (body stmt)
tce = typecheckExpr (ListType (Unspecfied "u")) (signature stmt)
--tc = Typecheck.typecheck NumType (ListType (Unspecfied "x")) Map.empty

--tc = Typecheck.typecheck NumType (ListType (Unspecfied "x")) Map.empty
--tc = Typecheck.typecheck Num tl Map.empty

--h = Typecheck.typecheck t tl Map.empty
--h = Typecheck.typecheck (ListType $ Unspecfied "a") (ListType $ Unspecfied "b") Map.empty

stmt' = Function { name = "tail"
                , signature = Arrow (ListType (Unspecfied "x")) (ListType (Unspecfied "x"))
                , args = ["xs"]
                , body = TernOp Slice (Ident "xs") (Val $ Number 1) (UnOp Length (Ident "xs"))
                }
m' = Map.singleton "tail" stmt
x = typeofExpr m stmt (body stmt')

main :: IO ()
main = do
  print x
