{-# LANGUAGE BangPatterns, NamedFieldPuns #-}

module Typecheck where

import Data.Function
import Data.Functor
import Data.List (elemIndex, foldl', drop)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Either (lefts)
import Control.Applicative
import Data.Traversable
import Syntax
import CodeGen
import Util
import qualified Data.Map as Map

data TypeError
  = NotFunction Name  -- maybe make better show
  | TypeMismatch TypeExpr TypeExpr Integer
  | Unexpected
  deriving (Show, Eq)

data TypecheckError = TypecheckError Stmt TypeError
  deriving (Eq)

instance Show TypecheckError where
  show (TypecheckError Function { name, signature, args } typeError)
    = "Typecheck Error in function " ++ name ++ ": " ++ show typeError

typecheckModule :: Module -> Either [TypecheckError] Module
typecheckModule mod@(Module stmts)
  = if null typeErrors then Right mod else Left typeErrors
    where
      !typeErrors
        =  stmts
       <&> typecheckStmt (modToStmtMap mod)
        &  lefts

typecheckStmt :: Map Name Stmt -> Stmt -> Either TypecheckError TypeExpr
typecheckStmt stmtMap stmt@Function { body, args, signature } = do
  typeofBody <- typeofExpr stmtMap stmt body
  expectedTypeOfBody <- typeExprToList signature
                      & drop (length args)
                      & typeExprFromList
  typeEqual Map.empty expectedTypeOfBody typeofBody
  &   mapLeft (TypecheckError stmt)

typeEqual :: Map Name TypeExpr -> TypeExpr -> TypeExpr -> Either TypeError TypeExpr
typeEqual m te1@(Unspecfied a) te2@(Unspecfied b) =
  case Map.lookup a m of
    Nothing -> if te1 == te2 then Right te1 else Left $ TypeMismatch te1 te2 0
    Just te1 -> if te1 == te2 then Right te1 else Left $ TypeMismatch te1 te2 1
typeEqual m (ListType te1) (ListType te2) = ListType <$> typeEqual m te1 te2
--typeEqual (TupType te1a te1b) (TupType te2a te2b) =
--typeEqual (Arrow TypeExpr TypeExpr)
typeEqual m te1 te2 = if te1 == te2 then Right te1 else Left $ TypeMismatch te1 te2 2

modToStmtMap :: Module -> Map Name Stmt
modToStmtMap (Module stms)
  =  stms
 <&> (\func@Function { name } -> (name, func))
  &  Map.fromList

typecheckExpr :: TypeExpr -> TypeExpr -> Either TypeError TypeExpr
typecheckExpr t (Arrow tl tr)
  = typecheck t tl Map.empty  -- compare type with sig
  <&> resolveType tr
typecheckExpr t1 t2 = Left $ TypeMismatch t1 t2 3

typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
typecheck t (Unspecfied n) m =
  case Map.lookup n m of
    Nothing -> Right $ Map.insert n t m
    Just t' -> if t == t' then Right m else Left $ TypeMismatch t t' 4
typecheck (Arrow t0 t1) (Arrow t2 t3) m = typecheck t0 t2 m >>= typecheck t1 t3
typecheck (TupType te0 te1) (TupType te2 te3) m = typecheck te0 te2 m >>= typecheck te1 te3
typecheck (ListType te0) (ListType te1) m = typecheck te0 te1 m
typecheck t1 t2 m =
  if t1 == t2 then Right m
              else Left $ TypeMismatch t1 t2 5

resolveType :: TypeExpr -> Map Name TypeExpr -> TypeExpr
resolveType t@(Unspecfied n) m = fromMaybe t (Map.lookup n m)
resolveType t@(TupType t1 t2) m = TupType (resolveType t1 m) (resolveType t2 m)
resolveType (ListType t) m = ListType (resolveType t m)
resolveType (Arrow tl tr) m = Arrow (resolveType tl m) (resolveType tr m)
resolveType t _ = t

typeExprToList :: TypeExpr -> [TypeExpr]
typeExprToList (Arrow t0 t1) = t0 : typeExprToList t1
typeExprToList t = [t]

typeExprFromList :: [TypeExpr] -> Either TypeError TypeExpr
typeExprFromList [te] = Right te
typeExprFromList (te:tes) = Arrow te <$> typeExprFromList tes
typeExprFromList [] = Left Unexpected

argToMaybeSig :: Name -> [Name] -> TypeExpr -> Maybe TypeExpr
argToMaybeSig arg args sig
  =   elemIndex arg args
  >>= (!?) (typeExprToList sig)

typeofExpr :: Map Name Stmt -> Stmt -> Expr -> Either TypeError TypeExpr
typeofExpr m s (Val p) =
  case p of
    Number n -> Right NumType
    Character c -> Right CharType
    Atom a -> Right AtomType
    Tuple expr1 expr2 -> sequence [typeofExpr m s expr1, typeofExpr m s expr2]
      <&> \[e1, e2] -> TupType e1 e2
    List typeExpr exprs -> Right $ ListType typeExpr

typeofExpr m Function { signature = sig, args } (Ident name)
   =  argToMaybeSig name args sig
  <|> (Map.lookup name m <&> signature)
  <&> Right
   &  fromMaybe (Left $ NotFunction name)

typeofExpr m f@Function { signature = sig, args } (Call name exprs)
  =  argToMaybeSig name args sig
 <|> (Map.lookup name m <&> signature)
 <&> (\s ->  traverse (typeofExpr m f) exprs
         >>= foldl' (\s t -> s >>= typecheckExpr t) (Right s)
     )
  &  fromMaybe (Left $ NotFunction name)

typeofExpr m s (UnOp unop expr)
  = typeofExpr m s expr
  >>= \t -> typecheckExpr t (typeofUnOp unop)

typeofExpr m s (BinOp bop expr1 expr2)
  = sequence [typeofExpr m s expr1, typeofExpr m s expr2]
  >>= \[t1, t2] ->
    typecheckExpr t1 (typeofBop bop)
    >>= typecheckExpr t2

typeofExpr m s (TernOp top expr1 expr2 expr3)
  = sequence (typeofExpr m s <$> [expr1, expr2, expr3])
  >>= \[t1, t2, t3] ->
    typecheckExpr t1 (typeofTop top)
    >>= typecheckExpr t2
    >>= typecheckExpr t3

typeofUnOp :: UnOp -> TypeExpr
typeofUnOp Fst = Arrow (TupType (Unspecfied "a") (Unspecfied "b")) (Unspecfied "a")
typeofUnOp Snd = Arrow (TupType (Unspecfied "a") (Unspecfied "b")) (Unspecfied "b")

typeofBop :: Bop -> TypeExpr
typeofBop bop =
  case bop of
    Plus -> nnn
    Minus -> nnn
    Divide -> nnn
    Times -> nnn
    Equal -> Arrow (Unspecfied "a") (Arrow (Unspecfied "b") AtomType)
    GreaterThan -> nnb
    GreaterThanOrEqual -> nnb
    Rem -> nnb
    LessThan -> nnb
    LessThanOrEqual -> nnb
    Cons -> Arrow (Unspecfied "a") (Arrow (ListType (Unspecfied "a")) (ListType (Unspecfied "a")))
  where
    nnn = Arrow NumType (Arrow NumType NumType)
    nnb = Arrow NumType (Arrow NumType AtomType)

typeofTop :: Top -> TypeExpr
typeofTop Uncons = Arrow (ListType (Unspecfied "a")) (Arrow (Unspecfied "b") (Arrow (Arrow (Unspecfied "a") (Arrow (ListType (Unspecfied "a")) (Unspecfied "b"))) (Unspecfied "b")))
typeofTop If = Arrow AtomType (Arrow (Unspecfied "a") (Arrow (Unspecfied "a") (Unspecfied "a")))
