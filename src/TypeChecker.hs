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
typecheckExpr expr (Arrow t0 t1)
  = typeofExpr expr -- get type of expression
  >>= flip (typecheck t0) Map.empty  -- compare type with sig
  <&> applyTypeMap t1

typecheck :: TypeExpr -> TypeExpr -> Map Name Name -> Either TypeError (Map Name Name)
typecheck (Unspecfied a) (Unspecfied b) m =
  case Map.lookup a m of
    Nothing -> Right $ Map.insert a b m
    Just b -> if a == b then Right m else Left Mismatch
typecheck (Arrow t1 t2) (Arrow t3 t4) m = typecheck t1 t3 m >>= typecheck t2 t3
typecheck t1 t2 m =
  if t1 == t2 then Right m
              else Left Mismatch

applyTypeMap :: TypeExpr -> Map Name Name -> TypeExpr
applyTypeMap t@(Unspecfied a) m =
  case Map.lookup a m of
    Nothing -> t
    Just name -> Unspecfied name
applyTypeMap (Arrow tl tr) m = Arrow (applyTypeMap tl m) (applyTypeMap tr m)
applyTypeMap t _ = t

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
    nnn = Arrow NumType (Arrow NumType NumType)
    nnb = Arrow NumType (Arrow NumType AtomType)

