{-# LANGUAGE BangPatterns, NamedFieldPuns #-}

module Typecheck where

import Data.Function
import Data.Functor
import Data.List (elemIndex, foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Traversable
import Syntax
import qualified Data.Map as Map

data TypeError
  = NotFunction
  | BadGuardPredicate
  | AritySignatureMismatch
  | TypeMismatch
  deriving (Eq, Show)
  -- | NotInScope Name

typecheckStmt :: Map Name Statement -> Name -> Either TypeError TypeExpr
typecheckStmt = undefined

typecheckExpr :: TypeExpr -> TypeExpr -> Either TypeError TypeExpr
typecheckExpr t (Arrow tl tr)
  = typecheck t tl Map.empty  -- compare type with sig
  <&> applyTypeMap tr
typecheckExpr _ _ = Left NotFunction -- we cannot apply expr(s) any non-arrow type

typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
typecheck t (Unspecfied n) m =
  case Map.lookup n m of
    Nothing -> Right $ Map.insert n t m
    Just t' -> if t == t' then Right m else Left TypeMismatch
typecheck (Arrow t0 t1) (Arrow t2 t3) m = typecheck t0 t2 m >>= typecheck t1 t3
typecheck t1 t2 m =
  if t1 == t2 then Right m
              else Left TypeMismatch

applyTypeMap :: TypeExpr -> Map Name TypeExpr -> TypeExpr
applyTypeMap t@(Unspecfied n) m = fromMaybe t (Map.lookup n m)
applyTypeMap (Arrow tl tr) m = Arrow (applyTypeMap tl m) (applyTypeMap tr m)
applyTypeMap t _ = t

getArgTypeByIndex :: TypeExpr -> Int -> Maybe TypeExpr
getArgTypeByIndex t 0 = Just t
getArgTypeByIndex (Arrow t0 t1) n = getArgTypeByIndex t0 (n - 1)
getArgTypeByIndex _ _ = Nothing

typeofExpr :: (Map Name Statement) -> Statement -> Expr -> Either TypeError TypeExpr
typeofExpr _ _ (Val p) =
  case p of
    Number n -> Right NumType
    Character c -> Right CharType
    Atom a -> Right AtomType
typeofExpr _ (Statement { signature, args }) (Ident name)
   =  elemIndex (Var name) args
  >>= getArgTypeByIndex signature
  <&> Right
   &  fromMaybe (Left AritySignatureMismatch)
typeofExpr m _ (Call name exprs) =
  case Map.lookup name m of
    Nothing -> Left NotFunction
    Just s@(Statement { signature })
      ->  traverse (typeofExpr m s) exprs
      >>= foldl' (\s t -> s >>= typecheckExpr t) (Right signature)
typeofExpr m s (Guard exprPairs) = goodPs >> goodEs
  where
    (predicates, exprs) = unzip exprPairs
    goodPs =   predicates
          <&>  typeofExpr m s
           &   sequence
           >>= \(p:ps) -> if all (==AtomType) (p:ps) then Right p else Left BadGuardPredicate
    goodEs =   exprs
          <&>  typeofExpr m s
           &   sequence
           >>= \(t:ts) -> if all (==t) ts then Right t else Left TypeMismatch
typeofExpr m s (BinOp bop expr1 expr2)
  = sequence [typeofExpr m s expr1, typeofExpr m s expr2]
  >>= \[t1, t2] ->
    typecheckExpr t1 (typeofBop bop)
    >>= typecheckExpr t2
typeofExpr m s (TernOp top expr1 expr2 expr3)
  = sequence [typeofExpr m s expr1, typeofExpr m s expr2, typeofExpr m s expr3]
  >>= \[t1, t2, t3] ->
    typecheckExpr t1 (typeofTop top)
    >>= typecheckExpr t2
    >>= typecheckExpr t3

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

typeofTop :: Top -> TypeExpr
--typeofTop Fold = Arrow (Arrow (Unspecfied "b") (Arrow (Unspecfied "a") (Unspecfied "b"))) (Arrow (Unspecfied "b") (Arrow ((ListType (Unspecfied "a"))) (Unspecfied "b")))
typeofTop Fold = undefined
--typeofTop Unfold = Arrow (Arrow (Unspecfied "a") AtomType) (Arrow (Arrow (Unspecfied "a") (Unspecfied "a")) (Arrow (Unspecfied "a") (Unspecfied "a")))
typeofTop Unfold = undefined
-- unfoldTree :: (b -> Maybe (b, a, b)) -> b -> Tree a
-- typeofTop Unfold = (b -> b) -> [a, [b, b]] -> b -> TreeType a