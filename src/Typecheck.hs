{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Typecheck where

import Control.Applicative
import Data.Either (lefts)
import Data.Function
import Data.Functor
import Data.List (drop, elemIndex, foldl', isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybe)
import Data.Traversable
import Parser
import Syntax
import Utils

data TypeError
    = NotFunction Name
    | StreamNotFound Name
    | DuplicateFunction
    | TypeMismatch {expected :: TypeExpr, actual :: TypeExpr, typeMap :: Map Name TypeExpr}
    | ArityMismatch
    | RecursiveType [Name]
    | BadForall [Name]
    deriving (Show, Ord, Eq)

data TypecheckError = FunctionError Function TypeError | PipeError Pipe TypeError | DuplicateStreamError Stream
    deriving (Eq, Ord)

instance Show TypecheckError where
    show (FunctionError Function{name, signature, args} typeError) =
        unwords [show typeError, "in function", name]
    show (PipeError pipe typeError) =
        unwords [show typeError, "in", show pipe]
    show (DuplicateStreamError Stream{streamName}) =
        unwords ["DuplicateStream", streamName]

typecheckModule :: Module -> Either [TypecheckError] Module
typecheckModule mod@Module{functions, functionMap, streams, streamMap, pipes}
    | (not . null) typeErrors = Left typeErrors
    | (not . null) duplicateFuncs = Left $ duplicateFuncs <&> (`FunctionError` DuplicateFunction)
    | (not . null) duplicateStreams = Left $ duplicateStreams <&> DuplicateStreamError
    | (not . null) pipeErrors = Left pipeErrors
    | otherwise = Right mod
  where
    typeErrors :: [TypecheckError]
    typeErrors =
        functions
            <&> typecheckFunc functionMap
                & lefts
    duplicateFuncs :: [Function]
    duplicateFuncs = dupesOn name functions
    duplicateStreams :: [Stream]
    duplicateStreams = dupesOn streamName streams
    pipeErrors :: [TypecheckError]
    pipeErrors =
        pipes
            <&> typecheckPipe streamMap functionMap
                & lefts

toPipeError :: Pipe -> TypecheckError -> TypecheckError
toPipeError pipe (FunctionError _ te) = PipeError pipe te
toPipeError _ tce = tce

typecheckPipe :: Map Name Stream -> Map Name Function -> Pipe -> Either TypecheckError TypeExpr
typecheckPipe streamMap funcMap pipe@Pipe{funcName, inStreams, outStreamName} =
    pipeFunction
        >>= typecheckFunc funcMap
        & mapLeft (toPipeError pipe)
  where
    inStreamNames = fmap fst inStreams
    streams :: Either TypecheckError [Stream]
    streams = traverse (lookup' streamMap (PipeError pipe . StreamNotFound)) (inStreamNames ++ [outStreamName])
    expectedTypeExpr :: Either TypecheckError TypeExpr
    expectedTypeExpr = streams >>= mapLeft (PipeError pipe) . typeExprFromList . fmap streamSig
    pipeFunction :: Either TypecheckError Function
    pipeFunction = Function funcName <$> expectedTypeExpr <*> pure inStreamNames <*> pure (Call funcName (Ident <$> inStreamNames))

typecheckFunc :: Map Name Function -> Function -> Either TypecheckError TypeExpr
typecheckFunc funcMap func@Function{name, body, args, signature} =
    do
        (typeMap, typeofBody) <- typeofExpr funcMap func body
        expectedTypeOfBody <-
            typeExprToList signature
                & drop (length args)
                & typeExprFromList
        checkForall name typeMap >> typeEqual typeMap expectedTypeOfBody typeofBody
        & mapLeft (FunctionError func)

checkForall :: Name -> Map Name TypeExpr -> Either TypeError TypeExpr
checkForall name m =
    Map.toList m
        & filter
            ( \case
                (_, Unspecified _) -> False
                (key, _) -> (name ++ ".") `isPrefixOf` key
            )
        & \case
            [] -> Right $ Unspecified ""
            errs -> Left $ BadForall (fst <$> errs)

typeEqual :: Map Name TypeExpr -> TypeExpr -> TypeExpr -> Either TypeError TypeExpr
typeEqual m te1@(Unspecified a) te2@(Unspecified b) =
    if te1 == te2
        then Right te1
        else case Map.lookup a m of
            Nothing -> Left $ TypeMismatch te1 te2 m
            Just t -> typeEqual m t te2
typeEqual m (ListType te1) (ListType te2) = ListType <$> typeEqual m te1 te2
typeEqual m (TupType te1 te2) (TupType te3 te4) = liftA2 TupType (typeEqual m te1 te3) (typeEqual m te2 te4)
typeEqual m te1 te2 = if te1 == te2 then Right te1 else Left $ TypeMismatch te1 te2 m

typecheckExpr :: Map Name TypeExpr -> TypeExpr -> TypeExpr -> Either TypeError (Map Name TypeExpr, TypeExpr)
typecheckExpr m t (Arrow tl tr) =
    typecheck t tl m
        >>= \m ->
            if Map.null $ Map.filterWithKey existCycles m
                then Right (m, resolveType m tr)
                else Left (RecursiveType $ Map.keys m)
typecheckExpr m t1 t2 = Left ArityMismatch

-- TODO rewrite with foldTypeExpr
existCycles :: Name -> TypeExpr -> Bool
existCycles name t@(Unspecified n) = name == n
existCycles name (TupType t1 t2) = any (existCycles name) [t1, t2]
existCycles name (ListType t) = existCycles name t
existCycles name (Arrow tl tr) = any (existCycles name) [tl, tr]
existCycles name t = False

resolveType :: Map Name TypeExpr -> TypeExpr -> TypeExpr
resolveType m t@(Unspecified n) = fromMaybe t (Map.lookup n m)
resolveType m t = applyTypeExpr (resolveType m) t

typecheck :: TypeExpr -> TypeExpr -> Map Name TypeExpr -> Either TypeError (Map Name TypeExpr)
typecheck t1 t2@(Unspecified n) m =
    if t1 == t2
        then Right m
        else case Map.lookup n m of
            Nothing -> Right $ Map.insert n t1 m
            Just t' -> if t1 == t' then Right m else Left $ TypeMismatch t1 t' m
typecheck (Arrow t0 t1) (Arrow t2 t3) m = typecheck t0 t2 m >>= typecheck t1 t3
typecheck (TupType te0 te1) (TupType te2 te3) m = typecheck te0 te2 m >>= typecheck te1 te3
typecheck (ListType te0) (ListType te1) m = typecheck te0 te1 m
typecheck t1 t2 m =
    if t1 == t2
        then Right m
        else Left $ TypeMismatch t1 t2 m

typeExprToList :: TypeExpr -> [TypeExpr]
typeExprToList (Arrow t0 t1) = t0 : typeExprToList t1
typeExprToList t = [t]

typeExprFromList :: [TypeExpr] -> Either TypeError TypeExpr
typeExprFromList [te] = Right te
typeExprFromList (te : tes) = Arrow te <$> typeExprFromList tes
typeExprFromList [] = Left ArityMismatch

argToMaybeSig :: Name -> [Name] -> TypeExpr -> Maybe TypeExpr
argToMaybeSig arg args sig =
    elemIndex arg args
        >>= (!?) (typeExprToList sig)

typeofExpr :: Map Name Function -> Function -> Expr -> Either TypeError (Map Name TypeExpr, TypeExpr)
typeofExpr m f (Val p) =
    case p of
        Number n -> Right (Map.empty, NumType)
        Byte c -> Right (Map.empty, ByteType)
        Atom a -> Right (Map.empty, AtomType)
        Tuple expr1 expr2 ->
            mapM (typeofExpr m f) [expr1, expr2]
                <&> \[(m1, e1), (m2, e2)] -> (m1 `Map.union` m2, TupType e1 e2)
        List typeExpr exprs ->
            mapM (typeofExpr m f) exprs
                <&> filter ((/= typeExpr) . snd)
                & \case
                    Left err -> Left err
                    Right [] -> Right (Map.empty, ListType typeExpr)
                    Right ((m, te) : _) -> Left $ TypeMismatch typeExpr te m
typeofExpr m f@Function{signature = sig, args} e =
    case e of
        Ident name -> maybe (Left $ NotFunction name) (Right . (Map.empty,)) (maybeSig name)
        Call name exprs -> maybe (Left $ NotFunction name) (check exprs) (maybeSig name)
        UnOp unop expr1 -> check [expr1] (typeofUnOp unop)
        BinOp bop expr1 expr2 -> check [expr1, expr2] (typeofBop bop)
        TernOp top expr1 expr2 expr3 -> check [expr1, expr2, expr3] (typeofTop top)
  where
    maybeSig :: String -> Maybe TypeExpr
    maybeSig name = argToMaybeSig name args sig <|> (signature <$> Map.lookup name m)
    check :: [Expr] -> TypeExpr -> Either TypeError (Map Name TypeExpr, TypeExpr)
    check es sig =
        mapM (typeofExpr m f) es
            >>= foldl' (\ms t -> ms >>= \(m, s) -> typecheckExpr m t s) (Right (Map.empty, sig)) . fmap snd

typeofUnOp :: UnOp -> TypeExpr
typeofUnOp Fst = alphaConversion "fst" $ Arrow (TupType (Unspecified "a") (Unspecified "b")) (Unspecified "a")
typeofUnOp Snd = alphaConversion "snd" $ Arrow (TupType (Unspecified "a") (Unspecified "b")) (Unspecified "b")

typeofBop :: Bop -> TypeExpr
typeofBop bop =
    case bop of
        Plus -> nnn
        Minus -> nnn
        Divide -> nnn
        Times -> nnn
        Equal -> alphaConversion "eq" $ Arrow (Unspecified "a") (Arrow (Unspecified "b") AtomType)
        GreaterThan -> nnb
        GreaterThanOrEqual -> nnb
        Rem -> nnb
        LessThan -> nnb
        LessThanOrEqual -> nnb
        Cons -> alphaConversion "cons" $ Arrow (Unspecified "a") (Arrow (ListType (Unspecified "a")) (ListType (Unspecified "a")))
  where
    nnn = Arrow NumType (Arrow NumType NumType)
    nnb = Arrow NumType (Arrow NumType AtomType)

typeofTop :: Top -> TypeExpr
typeofTop Uncons = alphaConversion "uncons" $ Arrow (ListType (Unspecified "a")) (Arrow (Unspecified "b") (Arrow (Arrow (Unspecified "a") (Arrow (ListType (Unspecified "a")) (Unspecified "b"))) (Unspecified "b")))
typeofTop If = alphaConversion "if" $ Arrow AtomType (Arrow (Unspecified "a") (Arrow (Unspecified "a") (Unspecified "a")))
