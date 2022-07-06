{-# LANGUAGE NamedFieldPuns #-}

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
  | DuplicateFunction
  | TypeMismatch TypeExpr TypeExpr String
  | ArityMismatch
  deriving (Show, Eq)

data TypecheckError = TypecheckError Function TypeError
  deriving (Eq)

instance Show TypecheckError where
  show (TypecheckError Function { name, signature, args } typeError)
    = "Typecheck Error in function " ++ name ++ ": " ++ show typeError

typecheckModule :: Module -> Either [TypecheckError] Module
typecheckModule mod@Module { functions, functionMap }
  | (not . null) typeErrors = Left typeErrors
  | (not . null) duplicateFuncs = Left $ duplicateFuncs <&> (`TypecheckError` DuplicateFunction)
  | otherwise = Right mod
    where
      typeErrors :: [TypecheckError]
      typeErrors
        =  functions
       <&> typecheckFunc functionMap
        &  lefts
      duplicateFuncs :: [Function]
      duplicateFuncs = dupesOn name functions

typecheckFunc :: Map Name Function -> Function -> Either TypecheckError TypeExpr
typecheckFunc funcMap func@Function { body, args, signature } = do
  typeofBody <- typeofExpr funcMap func body
  expectedTypeOfBody <- typeExprToList signature
                      & drop (length args)
                      & typeExprFromList
  typeEqual expectedTypeOfBody typeofBody
  &   mapLeft (TypecheckError func)

typeEqual :: TypeExpr -> TypeExpr -> Either TypeError TypeExpr
typeEqual te1@(Unspecfied a) (Unspecfied b) = Right te1
typeEqual (ListType te1) (ListType te2) = ListType <$> typeEqual te1 te2
typeEqual (TupType te1 te2) (TupType te3 te4) = liftA2 TupType (typeEqual te1 te3) (typeEqual te2 te4)
typeEqual te1 te2 = if te1 == te2 then Right te1 else Left $ TypeMismatch te1 te2 "typeEqual"

typecheckExpr :: TypeExpr -> TypeExpr -> Either TypeError TypeExpr
typecheckExpr t (Arrow tl tr)
  = typecheck t tl Map.empty  -- compare type with sig
  <&> resolveType tr
typecheckExpr t1 t2 = Left $ TypeMismatch t1 t2 "typecheckExpr"

typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
typecheck t (Unspecfied n) m =
  case Map.lookup n m of
    Nothing -> Right $ Map.insert n t m
    Just t' -> if t == t' then Right m else Left $ TypeMismatch t t' "typecheck"
typecheck (Arrow t0 t1) (Arrow t2 t3) m = typecheck t0 t2 m >>= typecheck t1 t3
typecheck (TupType te0 te1) (TupType te2 te3) m = typecheck te0 te2 m >>= typecheck te1 te3
typecheck (ListType te0) (ListType te1) m = typecheck te0 te1 m
typecheck t1 t2 m =
  if t1 == t2 then Right m
              else Left $ TypeMismatch t1 t2 "typecheck"

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
typeExprFromList [] = Left ArityMismatch

argToMaybeSig :: Name -> [Name] -> TypeExpr -> Maybe TypeExpr
argToMaybeSig arg args sig
  =   elemIndex arg args
  >>= (!?) (typeExprToList sig)

typeofExpr :: Map Name Function -> Function -> Expr -> Either TypeError TypeExpr
typeofExpr m s (Val p) =
  case p of
    Number n -> Right NumType
    Character c -> Right CharType
    Atom a -> Right AtomType
    Tuple expr1 expr2 -> sequence (typeofExpr m s <$> [expr1, expr2])
      <&> \[e1, e2] -> TupType e1 e2
    List typeExpr exprs -> Right $ ListType typeExpr

typeofExpr m f@Function { signature = sig, args } e =
  case e of
    Ident name ->
      maybeSignature name
      <&> Right
       &  fromMaybe (Left $ NotFunction name)
    Call name exprs ->
      maybeSignature name
      <&> check exprs
       &  fromMaybe (Left $ NotFunction name)
    UnOp unop expr1 -> check [expr1] (typeofUnOp unop)
    BinOp bop expr1 expr2 -> check [expr1, expr2] (typeofBop bop)
    TernOp top expr1 expr2 expr3 -> check [expr1, expr2, expr3] (typeofTop top)
  where
    maybeSignature :: String -> Maybe TypeExpr
    maybeSignature name = argToMaybeSig name args sig <|> (signature <$> Map.lookup name m)
    check :: [Expr] -> TypeExpr -> Either TypeError TypeExpr
    check es sig = traverse (typeofExpr m f) es
      >>= foldl' (\s t -> s >>= typecheckExpr t) (Right sig)

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
