module Test where

import Data.Function
import Data.Functor
import Syntax
import Typecheck
import Data.Map (Map)
import qualified Data.Map as Map


--typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
res = typecheck (Arrow (Unspecfied "a") (Unspecfied "a")) (Arrow (Unspecfied "b") (Unspecfied "b")) Map.empty



--id2 -> w -> [w] [w]
--id2 x xs = xs

id2 = Function { name = "id2"
               , signature = Arrow (Unspecfied "w") (Arrow (ListType (Unspecfied "w")) (ListType (Unspecfied "w")))
               , args = ["q", "qs"]
               , body = Ident "qs"
               }

-- tail -> [f] [f]
-- tail xs =
-- <+ xs xs id2
tail2 = Function { name = "tail2"
                 , signature = Arrow (ListType (Unspecfied "t")) (ListType (Unspecfied "t"))
                 , args = ["ts"]
                 , body = TernOp Uncons (Ident "ts") (Ident "ts") (Ident "id2")
                 }

--uncons    ([a] -> (b -> ((a -> ([a] -> b)) -> b)))
m = Map.fromList [("id2", id2), ("tail2", tail2)]

expectedTypeOfBody = typeExprToList (signature tail2)
                & drop (length (args tail2))
                & typeExprFromList

shouldBeT = typeofExpr m tail2 (body tail2)
t1 = typeofExpr m tail2 (Ident "ts")
t2 = typeofExpr m tail2 (Ident "ts")
--t3 = typeofExpr m tail2 (Ident "id2")

t3 = Arrow (Unspecfied "w") (Arrow (ListType (Unspecfied "w")) (ListType (Unspecfied "w")))



tc = typecheckExpr (ListType (Unspecfied "t")) (typeofTop Uncons)
    >>= typecheckExpr (ListType (Unspecfied "t"))

tc2 = tc >>= typecheckExpr t3



main :: IO ()
main = do
  print tc
  print t3
  print tc2
  --print $ typeofExpr m id2 (body id2)
  print "should be T"
  print shouldBeT
  --print "-----"
  --print e1
  --print e2
  --print $ typecheckStmt m id2
  --print $ typecheckStmt m tail2
  --print expectedTypeOfBody
