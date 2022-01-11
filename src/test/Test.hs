module Test where

import Data.Function
import Data.Functor
import Syntax
import Transpile
import Typecheck
import qualified Data.Map as Map


stmt = Function { name = "avg"
                , signature = Arrow NumType (Arrow NumType NumType)
                , args = ["num1", "num2"]
                , body = BinOp Times (BinOp Plus (Ident "num1") (Ident "num2")) (Val $ Number 0.5)
                }
m = Map.singleton "avg" stmt
typeofBody = typeofExpr m stmt (body stmt)
argsTypeExpr = typeExprToList (signature stmt)
             & take (length (args stmt))
             & typeExprFromList
expectedTypeOfBody = typecheckExpr argsTypeExpr (signature stmt)

main :: IO ()
main = do
  print typeofBody
  print argsTypeExpr
  print expectedTypeOfBody
