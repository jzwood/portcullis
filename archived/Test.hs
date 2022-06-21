module Test where

import Data.Function
import Data.Functor
import Syntax
import CodeGen
import Parser
import Typecheck
import Compile
import qualified Data.Map as Map

hof1 = Function { name = "hof1"
                          , signature = Arrow (Arrow (Unspecfied "a") AtomType) (Arrow (ListType NumType) (ListType NumType))
                          , args = ["f", "xs"]
                          , body = Guard [ ( Call "f" [Ident "xs"] , Ident "xs")
                                         ] (Ident "xs")
                          }

m = Map.fromList [("hof1", hof1)]
t0 = typeofExpr m hof1 (body hof1)
t = typecheckFunc m hof1

t1 = typeofExpr m hof1 (Call "f" [Ident "xs"])

main :: IO ()
main = do
  print t1
