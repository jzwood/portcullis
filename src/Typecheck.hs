{-# LANGUAGE NamedFieldPuns #-}

module Typecheck where

import Data.Function
import Data.Functor
import Data.List (elemIndex, foldl', drop)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Either (lefts)
import Control.Applicative
import Data.Traversable
import Syntax
import Parser
import CodeGen
import Util
import qualified Data.Map as Map

data TypeError
  = NotFunction Name
  | QueueNotFound Name
  | DuplicateFunction
  | TypeMismatch { expected :: TypeExpr, actual :: TypeExpr }
  | ArityMismatch
  | RecursiveType [(Name, TypeExpr)]
  deriving (Show, Ord, Eq)

data TypecheckError = FunctionError Function TypeError | PipeError Pipe TypeError | DuplicateQueueError Queue
  deriving (Eq, Ord)

instance Show TypecheckError where
  show (FunctionError Function { name, signature, args } typeError)
     = unwords [ show typeError, "in function", name ]
  show (PipeError pipe typeError)
     = unwords [ show typeError, "in", show pipe ]
  show (DuplicateQueueError Queue { queueName })
     = unwords [ "DuplicateQueue", queueName ]

typecheckModule :: Module -> Either [TypecheckError] Module
typecheckModule mod@Module { functions, functionMap, queues, queueMap, pipes }
  | (not . null) typeErrors = Left typeErrors
  | (not . null) duplicateFuncs = Left $ duplicateFuncs <&> (`FunctionError` DuplicateFunction)
  | (not . null) duplicateQueues = Left $ duplicateQueues <&> DuplicateQueueError
  | (not . null) pipeErrors = Left pipeErrors
  | otherwise = Right mod
    where
      typeErrors :: [TypecheckError]
      typeErrors
        =  functions
       <&> typecheckFunc functionMap
        &  lefts
      duplicateFuncs :: [Function]
      duplicateFuncs = dupesOn name functions
      duplicateQueues :: [Queue]
      duplicateQueues = dupesOn queueName queues
      pipeErrors :: [TypecheckError]
      pipeErrors =  pipes
                <&> typecheckPipe queueMap functionMap
                 &  lefts

toPipeError :: Pipe -> TypecheckError -> TypecheckError
toPipeError pipe (FunctionError _ te) = PipeError pipe te
toPipeError _ tce = tce

typecheckPipe :: Map Name Queue -> Map Name Function -> Pipe -> Either TypecheckError TypeExpr
typecheckPipe queueMap funcMap pipe@Pipe { funcName, inQueueNames, outQueueName }
  =   pipeFunction
  >>= typecheckFunc funcMap
  & mapLeft (toPipeError pipe)
  where
    queues :: Either TypecheckError [Queue]
    queues = traverse (lookup' queueMap (PipeError pipe . QueueNotFound)) (inQueueNames ++ [outQueueName])
    expectedTypeExpr :: Either TypecheckError TypeExpr
    expectedTypeExpr = queues >>= mapLeft (PipeError pipe) . typeExprFromList . fmap queueSig
    pipeFunction :: Either TypecheckError Function
    pipeFunction = Function funcName <$> expectedTypeExpr <*> pure inQueueNames <*> pure (Call funcName (Ident <$> inQueueNames))

typecheckFunc :: Map Name Function -> Function -> Either TypecheckError TypeExpr
typecheckFunc funcMap func@Function { body, args, signature } = do
  typeofBody <- typeofExpr funcMap func body
  expectedTypeOfBody <- typeExprToList signature
                      & drop (length args)
                      & typeExprFromList
  typeEqual expectedTypeOfBody typeofBody
  & mapLeft (FunctionError func)

typeEqual :: TypeExpr -> TypeExpr -> Either TypeError TypeExpr
typeEqual te1@(Unspecfied a) (Unspecfied b) = Right te1
typeEqual (ListType te1) (ListType te2) = ListType <$> typeEqual te1 te2
typeEqual (TupType te1 te2) (TupType te3 te4) = liftA2 TupType (typeEqual te1 te3) (typeEqual te2 te4)
typeEqual te1 te2 = if te1 == te2 then Right te1 else Left $ TypeMismatch te1 te2

typecheckExpr :: Map Name TypeExpr -> TypeExpr -> TypeExpr -> Either TypeError (Map Name TypeExpr, TypeExpr)
typecheckExpr m t (Arrow tl tr)
  =  typecheck t tl m
 >>= \m -> if Map.null $ Map.filterWithKey existCycles m
           then Right (m, resolveType m tr)
           else Left (RecursiveType (Map.toList m))
typecheckExpr m t1 t2 = Left $ TypeMismatch t1 t2

existCycles :: Name -> TypeExpr -> Bool
existCycles name t@(Unspecfied n) = name == n
existCycles name (TupType t1 t2) = any (existCycles name) [t1, t2]
existCycles name (ListType t) = existCycles name t
existCycles name (Arrow tl tr) = any (existCycles name) [tl, tr]
existCycles name t = False

resolveType :: Map Name TypeExpr -> TypeExpr -> TypeExpr
resolveType m t@(Unspecfied n) = fromMaybe t (Map.lookup n m)
resolveType m t = applyTypeExpr (resolveType m) t

typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
typecheck t1 t2@(Unspecfied n) m =
  if t1 == t2 then Right m else
  case Map.lookup n m of
    Nothing -> Right $ Map.insert n t1 m
    Just t' -> if t1 == t' then Right m else Left $ TypeMismatch t1 t'
typecheck (Arrow t0 t1) (Arrow t2 t3) m = typecheck t0 t2 m >>= typecheck t1 t3
typecheck (TupType te0 te1) (TupType te2 te3) m = typecheck te0 te2 m >>= typecheck te1 te3
typecheck (ListType te0) (ListType te1) m = typecheck te0 te1 m
typecheck t1 t2 m =
  if t1 == t2 then Right m
              else Left $ TypeMismatch t1 t2

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
typeofExpr m f (Val p) =
  case p of
    Number n -> Right NumType
    Character c -> Right CharType
    Atom a -> Right AtomType
    Tuple expr1 expr2 -> sequence (typeofExpr m f <$> [expr1, expr2])
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
    check es sig = mapM (typeofExpr m f) es -- rewrite with mapM
      >>= foldl' (\ms t -> ms >>= \(m, s) -> typecheckExpr m t s) (Right (Map.empty, sig))
      <&> snd

typeofUnOp :: UnOp -> TypeExpr
typeofUnOp Fst = alphaConversion "fst" $ Arrow (TupType (Unspecfied "a") (Unspecfied "b")) (Unspecfied "a")
typeofUnOp Snd = alphaConversion "snd" $ Arrow (TupType (Unspecfied "a") (Unspecfied "b")) (Unspecfied "b")

typeofBop :: Bop -> TypeExpr
typeofBop bop =
  case bop of
    Plus -> nnn
    Minus -> nnn
    Divide -> nnn
    Times -> nnn
    Equal -> alphaConversion "eq" $ Arrow (Unspecfied "a") (Arrow (Unspecfied "b") AtomType)
    GreaterThan -> nnb
    GreaterThanOrEqual -> nnb
    Rem -> nnb
    LessThan -> nnb
    LessThanOrEqual -> nnb
    Cons -> alphaConversion "cons" $ Arrow (Unspecfied "a") (Arrow (ListType (Unspecfied "a")) (ListType (Unspecfied "a")))
  where
    nnn = Arrow NumType (Arrow NumType NumType)
    nnb = Arrow NumType (Arrow NumType AtomType)

typeofTop :: Top -> TypeExpr
typeofTop Uncons = alphaConversion "uncons" $ Arrow (ListType (Unspecfied "a")) (Arrow (Unspecfied "b") (Arrow (Arrow (Unspecfied "a") (Arrow (ListType (Unspecfied "a")) (Unspecfied "b"))) (Unspecfied "b")))
typeofTop If = alphaConversion "if" $ Arrow AtomType (Arrow (Unspecfied "a") (Arrow (Unspecfied "a") (Unspecfied "a")))
