{-# LANGUAGE BangPatterns #-}

module TypeChecker where

import Data.Function
import Data.Functor
import qualified Data.Map as Map
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
  >>= \t0 -> applyType t0 t1 t2  -- compare type with sig

concretize :: (Name -> TypeExpr) -> TypeExpr -> TypeExpr
concretize f (Arrow t1 t2) = Arrow (concretize f t1) (concretize f t2)
concretize f (Unspecfied name) = f name
concretize f t = t

normalizeTypeExpr :: Map Name Name -> TypeExpr -> (TypeExpr, Map Name Name)
normalizeTypeExpr m (Unspecfied a) =
  case Map.lookup a m of
    Nothing -> (Unspecfied a', Map.insert a a' m)
    Just a -> (Unspecfied a, m)
  where
    !a' = 'a' : (show $ Map.size m)
normalizeTypeExpr m (Arrow t1 t2) = (Arrow nt1 nt2, nm2)
  where
    (!nt1, !nm1) = normalizeTypeExpr m t1
    (!nt2, !nm2) = normalizeTypeExpr nm1 t2
normalizeTypeExpr m t = (t, m)

-- t0 is type being applied
-- t1 t2 are the decomposed arrow type that's checked against (t1 -> t2)
-- t0 must be compatible with t1
applyType :: TypeExpr -> TypeExpr -> TypeExpr -> Either TypeError TypeExpr
applyType t0 t1@(Unspecfied n1) t2 = Right $ concretize (\n0 -> if n0 == n1 then t0 else t1) t2
applyType (Arrow t1 t2) (Arrow t1' t2') _ = undefined
applyType (Arrow _ _) _ _ = Left Mismatch
applyType _ (Arrow _ _) _ = Left Mismatch
applyType t0 t1 t2
  | t0 == t1 = Right t2
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

