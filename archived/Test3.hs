module Test where

import Data.Function
import Data.Functor
import Syntax
import Typecheck
import Data.Map (Map)
import qualified Data.Map as Map


--typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
res = typecheck (Arrow (Unspecified "a") (Unspecified "a")) (Arrow (Unspecified "b") (Unspecified "b")) Map.empty



--id2 -> w -> [w] [w]
--id2 x xs = xs

id2 = Function { name = "id2"
               , signature = Arrow (Unspecified "w") (Arrow (ListType (Unspecified "w")) (ListType (Unspecified "w")))
               , args = ["q", "qs"]
               , body = Ident "qs"
               }

-- tail -> [f] [f]
-- tail xs =
-- <+ xs xs id2
tail2 = Function { name = "tail2"
                 , signature = Arrow (ListType (Unspecified "t")) (ListType (Unspecified "t"))
                 , args = ["ts"]
                 , body = TernOp Uncons (Ident "ts") (Ident "ts") (Ident "id2")
                 }

--uncons    ([a] -> (b -> ((a -> ([a] -> b)) -> b)))
m = Map.fromList [("id2", id2), ("tail2", tail2)]

expectedTypeOfBody = typeExprToList (signature tail2)
                & drop (length (args tail2))
                & typeExprFromList

shouldBeT = typeofExpr m tail2 (body tail2)
--t1 = typeofExpr m tail2 (Ident "ts")
--t2 = typeofExpr m tail2 (Ident "ts")
--t3 = typeofExpr m tail2 (Ident "id2")

t3 = Arrow (Unspecified "w") (Arrow (ListType (Unspecified "w")) (ListType (Unspecified "w")))

tc = typecheckExpr (ListType (Unspecified "t")) (typeofTop Uncons)
  >>= typecheckExpr (ListType (Unspecified "t"))

--tc2 = t3 >>= typecheckExpr tc


---------------------------------------

-- map -> -> t g -> t g
-- map f a = (f a)
ma = Function { name = "ma"
              , signature = Arrow (Arrow (Unspecified "t") (Unspecified "g")) (Arrow (Unspecified "t") (Unspecified "g"))
              , args = ["f", "h"]
              , body = Call "f" [Ident "h"]
              }


-- mplus -> -> x y -> x y
-- mplus f p = (map f p)
ma2 = Function { name = "ma2"
               , signature = Arrow (Arrow (Unspecified "x") (Unspecified "y")) (Arrow (Unspecified "x") (Unspecified "y"))
               , args = ["b", "n"]
               , body = Call "ma" [Ident "b", Ident "n"]
               }

mamap = Map.fromList [("ma", ma), ("ma2", ma2)]


{-
typeofExpr m f@Function { signature = sig, args } (Call name exprs)
  =  argToMaybeSig name args sig
 <|> (signature <$> Map.lookup name m)
 <&> (\s ->  traverse (typeofExpr m f) exprs
         >>= foldl' (\s t -> s >>= typecheckExpr t) (Right s)
     )
  &  fromMaybe (Left $ NotFunction name)
-}

main :: IO ()
main = do
  --print $ typeofExpr mamap ma (body ma)
  --
  --
  --
  --print tc
  --print t3
  --print tc2
  --print $ typeofExpr m id2 (body id2)
  putStrLn "\n-----"
  putStrLn "should be [t]"
  print shouldBeT
