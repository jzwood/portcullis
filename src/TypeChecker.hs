module TypeChecker where

import Data.Function
import Data.Functor
import Data.Map (Map)
import Data.Traversable
import Data.List
import Syntax


data TypeError
  = NotFunction
  | BadGuardPredicate
  | Mismatch
  deriving (Eq, Show)
  -- | Mismatch TypeExpr TypeExpr
  -- | NotFunction TypeExpr
  -- | NotInScope Name

typecheckExpr :: Expr -> TypeExpr -> Either TypeError TypeExpr
typecheckExpr _ NumType = Left NotFunction -- we cannot apply expr(s) to numtype
typecheckExpr _ CharType = Left NotFunction -- we cannot apply expr(s) to numtype
typecheckExpr _ AtomType = Left NotFunction -- we cannot apply expr(s) to numtype
typecheckExpr expr (Arrow t1 t2)
  = typeofExpr expr -- get type of expression
  >>= typeCompare t1  -- compare type with sig
  >> Right t2

typeCompare :: TypeExpr -> TypeExpr -> Either TypeError TypeExpr
typeCompare t1 t2
  | t1 == t2 = Right t2
  | otherwise = Left Mismatch

typeofExpr :: Expr -> Either TypeError TypeExpr
typeofExpr (Prim p) =
  case p of
    Number n -> Right NumType
    Character c -> Right CharType
    Atom a -> Right AtomType
typeofExpr (Ident name) = undefined
typeofExpr (Call name exprs) = undefined
typeofExpr (Guard exprPairs) = goodPs >> goodEs
  where
    (predicates, exprs) = unzip exprPairs
    goodPs =   predicates
          <&>  typeofExpr
           &   sequence
           >>= \(p:ps) -> if all (==AtomType) (p:ps) then Right p else Left BadGuardPredicate
    goodEs =  exprs
          <&> typeofExpr
           &  sequence
      >>= \(t:ts) -> if all (==t) ts then Right t else Left Mismatch

typeofExpr (BinOp bop expr1 expr2)
  = typecheckExpr expr1 (typeofBop bop)
  >>= typecheckExpr expr2

typeofBop :: Bop -> TypeExpr
typeofBop bop =
  case bop of
    Plus -> nnn
    Minus -> nnn
    Divide -> nnn
    Times -> nnn
    Mod -> nnn
    Equal -> nnb
    NotEqual -> nnb
    GreaterThan -> nnb
    LessThan -> nnb
  where
    nnn = Arrow (NumType) (Arrow NumType NumType)
    nnb = Arrow (NumType) (Arrow NumType AtomType)

