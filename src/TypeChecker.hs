module TypeChecker where

import Syntax


data Type = TypesCheck | TypesDoNotCheck String

--data TypeExpr
  -- = NumType Name
  -- | Arrow TypeExpr TypeExpr
  -- deriving (Eq)

typecheck :: TypeExpr -> [TypeExpr] -> Either String TypeExpr
typecheck typeExpr typeExprs = undefined

typecheckBinOp :: Bop -> Expr -> Expr -> Either String TypeExpr
typecheckBinOp bop expr1 expr2 = typecheck (typeofBop bop) (typeofExpr <$> [expr1, expr2])

typeofExpr :: Expr -> TypeExpr
typeofExpr (Number n) = NumType "Num"
typeofExpr (Ident name) = undefined
typeofExpr (Call name exprs) = undefined
typeofExpr (Guard exprPairs) = undefined
--typeofExpr (BinOp bop expr1 expr2) = (typeofExpr expr1) (typeofBop) (typeofExpr)

typeofBop :: Bop -> TypeExpr
typeofBop Plus = Arrow (Arrow (NumType "Num") (NumType "Num")) (NumType "Num")
typeofBop Minus = Arrow (Arrow (NumType "Num") (NumType "Num")) (NumType "Num")
typeofBop Times = Arrow (Arrow (NumType "Num") (NumType "Num")) (NumType "Num")
typeofBop Divide = Arrow (Arrow (NumType "Num") (NumType "NonZero")) (NumType "Num")
typeofBop GreaterThan = Arrow (Arrow (NumType "Num") (NumType "Num")) (NumType "Bool")
typeofBop LessThan = Arrow (Arrow (NumType "Num") (NumType "Num")) (NumType "Bool")
typeofBop Equal = Arrow (Arrow (NumType "Num") (NumType "Num")) (NumType "Bool")
typeofBop NotEqual = Arrow (Arrow (NumType "Num") (NumType "Num")) (NumType "Bool")
typeofBop And = Arrow (Arrow (NumType "Bool") (NumType "Bool")) (NumType "Bool")
typeofBop Or = Arrow (Arrow (NumType "Bool") (NumType "Bool")) (NumType "Bool")
typeofBop Mod = Arrow (Arrow (NumType "Num") (NumType "Num")) (NumType "Num")
