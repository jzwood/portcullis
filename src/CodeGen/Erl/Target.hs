{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.Erl.Target where

import Prelude hiding (showList)
import Syntax
import CodeGen.Util (findAtoms)
import Data.Functor
import Data.Bifunctor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, intersperse, nub)
import Utils
import Data.Map (Map, (!))
import qualified Data.Map as Map

class Erl ast where
  toErl :: ast -> String

instance Erl Module where
  toErl Module { functions, comments, streamMap, pipes }
    = unlines $ (toErl <$> functions) ++ [atoms, zeroArityFuncs, topology]
      where
        atoms = unlines $ showAtoms functions
        zeroArityFuncs = unlines $ showZeroArityFunctions functions
        topology = showTopology streamMap pipes

instance Erl Function where
  toErl (Function name tExpr vars expr)
    = unlines
    [ docstring
    , unwords [header, "{"]
    , body
    , "}"
    ]
      where
        docstring = unwords ["// signature:", toErl tExpr]
        header = concat [ if null vars then "function $" else "export function " , name , (paren . head' "") vars ]
        body = (indent . concat) [ "return " , concatMap ((++ " => ") . paren) (tail' vars) , toErl expr , ";" ]

instance Erl TypeExpr where
  toErl NumType = "Num"
  toErl ByteType = "Byte"
  toErl AtomType = "Atom"
  toErl (Unspecified t) = t
  toErl (ListType t)
    = toErl t
    & bracket
  toErl (TupType t1 t2)
    =  [t1, t2]
   <&> toErl
    &  bracket . unwords
  toErl (Arrow tExpr1 tExpr2) = paren (toErl tExpr1 ++ pad "->" ++ toErl tExpr2)

instance Erl Value where
  toErl (Number n) = show n
  toErl (Byte b) = show b
  toErl (Atom n) = n
  toErl (List (Unspecified "") xs) = showList $ toErl <$> xs -- so uncons displays nicely
  toErl (List t xs) = unwords ["/*", toErl $ ListType t, "*/", showList $ toErl <$> xs]
  toErl (Tuple e1 e2)
    =  toErl <$> [e1, e2]
    &  showList

instance Erl Expr where
  toErl (Val p) = toErl p
  toErl (Ident name) = name
  toErl (Call name exprs) = name ++
    case exprs of
      [] -> ""  -- functions without arguments are interpreted as values
      _ -> concatMap (paren . toErl) exprs
  toErl (UnOp unop e) = toErl e ++ toErl unop
  toErl (BinOp bop e1 e2) = toErl $ Call (toErl bop) [e1, e2]
  toErl (TernOp If p e1 e2)
    = (paren . ('\n':) . (++"\n") . indent . unwords)
    [ toErl p ++ " ?"
    , toErl e1 ++ " :"
    , toErl e2
    ]
  toErl (TernOp Uncons xs b fb) =
    toErl $ TernOp If (BinOp Equal xs (Val $ List (Unspecified "") [])) b (Call (toErl fb) [UnOp Head xs, UnOp Tail xs])

instance Erl UnOp where
  toErl Fst = "[0]"
  toErl Snd = "[1]"
  toErl Head = ".at(0)"
  toErl Tail = ".slice(1)"

instance Erl Bop where
  toErl Plus = "_plus_"
  toErl Minus = "_minus_"
  toErl Times = "_mult_"
  toErl Divide = "_div_"
  toErl GreaterThan = "_gt_"
  toErl GreaterThanOrEqual = "_gte_"
  toErl LessThan = "_lt_"
  toErl LessThanOrEqual = "_lte_"
  toErl Rem = "_rem_"
  toErl Equal = "_eq_"
  toErl Cons = "_cons_"

showAtoms :: [Function] -> [String]
showAtoms funcs
  =  concatMap (findAtoms . body) funcs
  &  zip [0..] . nub . ("False" :) . ("True" :)
 <&> (\(i, atom) -> unwords ["const", atom, "=", show i] ++ ";")

showZeroArityFunctions :: [Function] -> [String]
showZeroArityFunctions funcs
  =  funcs
  &  filter (null . args)
 <&> name
 <&> (\name -> unwords ["export", "const", name, "=", '$' : name ++ "()" ])

showTopology :: Map Name Stream -> [Pipe] -> String
showTopology _ [] = "export const pipes = [];"
showTopology streamMap pipes
  =  pipes
 <&> showPipe streamMap
  &  ("export const pipes = " ++) . ("[\n" ++) . (++ "\n]") . indent . intercalate ",\n"

showPipe :: Map Name Stream -> Pipe -> String
showPipe streamMap Pipe { funcName, inStreams, outStreamName } =
  showList [funcName, showList (fmap (\(name, buffer) -> showList [show name, show buffer]) inStreams), show outStreamName]
