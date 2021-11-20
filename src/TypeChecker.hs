module TypeChecker where

import Syntax
import Data.Function
import Data.Functor
import Data.Map (Map)


data TypeError
  = WrongArity
  | Mismatch
  deriving (Eq, Show)
  -- | Mismatch TypeExpr TypeExpr
  -- | NotFunction TypeExpr
  -- | NotInScope Name

--data TypeExpr
  -- = NumType Name
  -- | Arrow TypeExpr TypeExpr
  -- deriving (Eq)

{-
  INSIGHT, MAYBE
  we can apply 1 expr at a time and get a new typeExpr each time
-}

typecheck :: TypeExpr -> [Expr] -> Either TypeError TypeExpr
typecheck t [] = Right t
typecheck (NumType _) _ = Left WrongArity -- we cannot apply expr(s) to numtype
typecheck (Arrow t1 t2) (e:es) =
  typeofExpr e <&>  -- get type of expression
  typeCompare t1 >>  -- compare type with sig
  typecheck t2 es  -- typecheck tail of sig against tail of expression

--typecheck :: Map Name TypeExpr -> Stmt -> Bool

--typecheckBinOp :: Bop -> Expr -> Expr -> Either TypeError TypeExpr
--typecheckBinOp bop expr1 expr2 =
  --if (typeofExpr expr1) == (NumType "Num") && (typeofExpr expr2) == (NumType "Num")
  --then Right (NumType "Num")
  --else Left "bin expresssions are wrong type"

typeCompare :: TypeExpr -> TypeExpr -> Bool
typeCompare = (==)

typeofExpr :: Expr -> Either TypeError TypeExpr
typeofExpr (Number n) = Right $ NumType "Num"
typeofExpr (Ident name) = undefined
typeofExpr (Call name exprs) = undefined
typeofExpr (Guard exprPairs) = undefined
--typeofExpr (BinOp bop expr1 expr2) = (typeCompare $ typeofBop bop) (Arrow expr1, expr2] -- typecheckBinOp bop expr1 expr2

typeofBop :: Bop -> TypeExpr
typeofBop bop =
  case bop of
    Plus -> numnumnum
    Minus -> numnumnum
    Divide -> numnumnum
    Times -> numnumnum
    Mod -> numnumnum
    Equal -> numnumbool
    NotEqual -> numnumbool
  where
    numnumnum = Arrow (NumType "Num") (Arrow (NumType "Num") (NumType "Num"))
    numnumbool = Arrow (NumType "Num") (Arrow (NumType "Num") (NumType "Bool"))

