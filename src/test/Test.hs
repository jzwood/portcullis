module Test where

import Data.Function
import Data.Functor
import Syntax
import Transpile
import Typecheck
import Data.List
import qualified Data.Map as Map

id1 = Function { name = "id1"
               , signature = Arrow (Unspecfied "z") (Unspecfied "z")
               , args = ["val"]
               , body = Ident "val"
               }
id2 = Function { name = "id2"
               , signature = Arrow (Unspecfied "p") (Unspecfied "p")
               , args = ["x"]
               , body = Call "id1" [Ident "x"]
               }

m = Map.fromList [("id1", id1), ("id2", id2)]

exprs = [Ident "x"]
a  = traverse (typeofExpr m id2) exprs
      >>= foldl' (\s t -> s >>= typecheckExpr t) (Right $ signature id1)

a' = traverse (typeofExpr m id1) exprs
      >>= foldl' (\s t -> s >>= typecheckExpr t) (Right $ signature id2)

b = typeofExpr m id2 (body id2)

main :: IO ()
main = do
  print a
  print a'
  print b
