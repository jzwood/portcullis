{-# LANGUAGE BangPatterns, NamedFieldPuns #-}

module Typecheck where

import Data.Function
import Data.Functor
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Traversable
import Syntax
import qualified Data.Map as Map

data TypeError
  = NotFunction
  | BadGuardPredicate
  | Mismatch
  deriving (Eq, Show)
  -- | Mismatch TypeExpr TypeExpr
  -- | NotFunction TypeExpr
  -- | NotInScope Name

typecheckExpr :: TypeExpr -> TypeExpr -> Either TypeError TypeExpr
typecheckExpr t (Arrow tl tr)
  = typecheck t tl Map.empty  -- compare type with sig
  <&> applyTypeMap tr
typecheckExpr _ _ = Left NotFunction -- we cannot apply expr(s) any non-arrow type

typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
typecheck t (Unspecfied n) m =
  case Map.lookup n m of
    Nothing -> Right $ Map.insert n t m
    Just t' -> if t == t' then Right m else Left Mismatch
typecheck (Arrow t0 t1) (Arrow t2 t3) m = typecheck t0 t2 m >>= typecheck t1 t3
typecheck t1 t2 m =
  if t1 == t2 then Right m
              else Left Mismatch

applyTypeMap :: TypeExpr -> Map Name TypeExpr -> TypeExpr
applyTypeMap t@(Unspecfied n) m = fromMaybe t (Map.lookup n m)
applyTypeMap (Arrow tl tr) m = Arrow (applyTypeMap tl m) (applyTypeMap tr m)
applyTypeMap t _ = t

typeofExpr :: (Map Name Statement) -> Statement -> Expr -> Either TypeError TypeExpr
typeofExpr _ _ (Prim p) =
  case p of
    Number n -> Right NumType
    Character c -> Right CharType
    Atom a -> Right AtomType
typeofExpr _ (Statement { signature, args }) (Ident name) = undefined
typeofExpr m _ (Call name exprs) =  -- signature expressions
  case Map.lookup name m of
    Nothing -> Left NotFunction
    Just (Statement { signature, args }) -> Right NumType  -- TODO find name in var
typeofExpr m s (Guard exprPairs) = goodPs >> goodEs
  where
    (predicates, exprs) = unzip exprPairs
    goodPs =   predicates
          <&>  typeofExpr m s
           &   sequence
           >>= \(p:ps) -> if all (==AtomType) (p:ps) then Right p else Left BadGuardPredicate
    goodEs =  exprs
          <&> typeofExpr m s
           &  sequence
          >>= \(t:ts) -> if all (==t) ts then Right t else Left Mismatch
typeofExpr m s (BinOp bop expr1 expr2)
  = sequence [typeofExpr m s expr1, typeofExpr m s expr2]
  >>= \[t1, t2] ->
    typecheckExpr t1 (typeofBop bop)
    >>= typecheckExpr t2

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

