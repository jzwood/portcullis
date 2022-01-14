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
import CompileTarget
import Util
import qualified Data.Map as Map

data TypeError
  = NotFunction
  | BadGuardPredicate [TypeExpr]
  | AritySignatureMismatch String
  | TypeMismatch [TypeExpr] String
  deriving (Show, Eq)

data TypecheckError = TypecheckError Stmt TypeError
  deriving (Eq)

instance Show TypecheckError where
  show (TypecheckError (Function { name, signature, args }) typeError)
    = "Typecheck Error in function " ++ name ++ ": " ++ show typeError

typecheckModule :: Module -> Either [TypecheckError] Module
typecheckModule mod@(Module stmts)
  = if null typeErrors then Right mod else Left typeErrors
    where
      !typeErrors
        =  stmts
       <&> (typecheckStmt $ modToStmtMap mod)
        &  lefts

typecheckStmt :: Map Name Stmt -> Stmt -> Either TypecheckError TypeExpr
typecheckStmt stmtMap stmt@(Function { body, args, signature }) = do
  typeofBody <- typeofExpr stmtMap stmt body
  cmp expectedTypeOfBody typeofBody
  &   mapLeft (TypecheckError stmt)
    where
      expectedTypeOfBody :: TypeExpr
      expectedTypeOfBody = typeExprToList signature
                  & drop (length args)
                  & typeExprFromList
      cmp :: TypeExpr -> TypeExpr -> Either TypeError TypeExpr
      cmp te1 te2 = if te1 == te2 then Right te1 else Left $ AritySignatureMismatch $ show ('A', te1, te2)

modToStmtMap :: Module -> Map Name Stmt
modToStmtMap (Module stms)
  =  stms
 <&> (\func@(Function { name }) -> (name, func))
  &  Map.fromList

typecheckExpr :: TypeExpr -> TypeExpr -> Either TypeError TypeExpr
typecheckExpr t (Arrow tl tr)
  = typecheck t tl Map.empty  -- compare type with sig
  <&> applyTypeMap tr
typecheckExpr t1 t2 = Left $ TypeMismatch [t1, t2] "typecheckExpr"

typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
typecheck t (Unspecfied n) m =
  case Map.lookup n m of
    Nothing -> Right $ Map.insert n t m
    Just t' -> if t == t' then Right m else Left $ TypeMismatch [t, t'] "typecheck a"
typecheck (Arrow t0 t1) (Arrow t2 t3) m = typecheck t0 t2 m >>= typecheck t1 t3
typecheck (TupType te0 te1) (TupType te2 te3) m = typecheck te0 te2 m >>= typecheck te1 te3
typecheck (ListType te0) (ListType te1) m = typecheck te0 te1 m
typecheck t1 t2 m =
  if t1 == t2 then Right m
              else Left $ TypeMismatch [t1, t2] ("typecheck b " ++ (show m))

applyTypeMap :: TypeExpr -> Map Name TypeExpr -> TypeExpr
applyTypeMap t@(Unspecfied n) m = fromMaybe t (Map.lookup n m)
applyTypeMap t@(TupType t1 t2) m = TupType (applyTypeMap t1 m) (applyTypeMap t2 m)
applyTypeMap (ListType t) m = ListType (applyTypeMap t m)
applyTypeMap (Arrow tl tr) m = Arrow (applyTypeMap tl m) (applyTypeMap tr m)
applyTypeMap t _ = t

typeExprToList :: TypeExpr -> [TypeExpr]
typeExprToList (Arrow t0 t1) = t0 : (typeExprToList t1)
typeExprToList t = [t]

typeExprFromList :: [TypeExpr] -> TypeExpr
typeExprFromList [te] = te
typeExprFromList (te:tes) = Arrow te (typeExprFromList tes)
typeExprFromList [] = error "typeExprFromList must be non-empty"

argToMaybeSig :: Name -> [Name] -> TypeExpr -> Maybe TypeExpr
argToMaybeSig arg args sig
  =   elemIndex arg args
  >>= (!?) (typeExprToList sig)

typeofExpr :: (Map Name Stmt) -> Stmt -> Expr -> Either TypeError TypeExpr
typeofExpr m s (Val p) =
  case p of
    Number n -> Right NumType
    Character c -> Right CharType
    Atom a -> Right AtomType
    Tuple expr1 expr2 -> sequence [typeofExpr m s expr1, typeofExpr m s expr2]
      <&> \[e1, e2] -> TupType e1 e2
    List typeExpr exprs -> Right $ ListType typeExpr
typeofExpr _ (Function { signature, args }) (Ident name)
   =  argToMaybeSig name args signature
  <&> Right
   &  fromMaybe (Left $ AritySignatureMismatch $ show ('B', name, args))
typeofExpr m f@(Function { signature = sig, args }) (Call name exprs)
  =  argToMaybeSig name args sig
 <|> (Map.lookup name m <&> signature)
 <&> (\s ->  traverse (typeofExpr m f) exprs
         >>= foldl' (\s t -> s >>= typecheckExpr t) (Right s)
     )
  &  fromMaybe (Left NotFunction)
typeofExpr m s (Guard cases defCase) = goodPs >> goodEs
  where
    (predicates, exprs) = unzip cases
    goodPs =   predicates
          <&>  typeofExpr m s
           &   sequence
           >>= \(p:ps) -> if all (==AtomType) (p:ps) then Right p else Left $ BadGuardPredicate (p:ps)
    goodEs =   exprs
          <&>  typeofExpr m s
           &   sequence
           >>= \(t:ts) -> if all (==t) ts then Right t else Left $ TypeMismatch (t:ts) "(typeofExpr)"
typeofExpr m s (UnOp unop expr)
  = typeofExpr m s expr
  >>= \t -> typecheckExpr t (typeofUnOp unop)
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

typeofUnOp :: UnOp -> TypeExpr
typeofUnOp Fst = Arrow (TupType (Unspecfied "a") (Unspecfied "b")) (Unspecfied "a")
typeofUnOp Snd = Arrow (TupType (Unspecfied "a") (Unspecfied "b")) (Unspecfied "b")
typeofUnOp Length = Arrow (ListType (Unspecfied "a")) NumType

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
    Concat -> Arrow (ListType (Unspecfied "a")) (Arrow (ListType (Unspecfied "a")) (ListType (Unspecfied "a")))
  where
    uub = Arrow (Unspecfied "a") (Arrow (Unspecfied "a") (Unspecfied "a"))
    nnn = Arrow NumType (Arrow NumType NumType)
    nnb = Arrow NumType (Arrow NumType AtomType)

typeofTop :: Top -> TypeExpr
typeofTop Slice = Arrow (ListType (Unspecfied "a")) (Arrow NumType (Arrow NumType (ListType (Unspecfied "a"))))
typeofTop At = Arrow (ListType (Unspecfied "a")) (Arrow NumType (Arrow (Unspecfied "a") (Unspecfied "a")))
