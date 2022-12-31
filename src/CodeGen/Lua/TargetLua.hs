{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.Lua.TargetLua where

import Syntax
import CodeGen.Util (findAtoms)
import Data.Functor
import Data.Bifunctor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, intersperse, nub)
import Util
import Data.Map (Map, (!))
import qualified Data.Map as Map

class Lua ast where
  toLua :: ast -> String

instance Lua Module where
  toLua Module { functions, comments, streamMap, pipes }
    = unlines $ (toLua <$> functions) ++ [atoms, zeroArityFuncs, topology]
      where
        atoms = unlines $ showAtoms functions
        zeroArityFuncs = unlines $ showZeroArityFunctions functions
        topology = showTopology streamMap pipes

instance Lua Function where
  toLua (Function name tExpr vars expr)
    = unlines [ docstring , header, body, foot ]
      where
        docstring = unwords ["-- signature:", toLua tExpr]
        header = concat [ if null vars then "function __" else "function " , name, (paren . head' "") vars]
        toLambda :: [String] -> Expr -> String
        toLambda [] expr = indent $ "return " ++ toLua expr
        toLambda (var:vars) expr = (indent . unlines) ["return function " ++ paren var, toLambda vars expr, "end" ]
        body = toLambda (tail' vars) expr
        foot = "end"

instance Lua TypeExpr where
  toLua NumType = "Num"
  toLua CharType = "Char"
  toLua AtomType = "Atom"
  toLua (Unspecified t) = t
  toLua (ListType t)
    = toLua t
    & bracket
  toLua (TupType t1 t2)
    =  [t1, t2]
   <&> toLua
    &  curly . unwords
  toLua (Arrow tExpr1 tExpr2) = paren (toLua tExpr1 ++ pad "->" ++ toLua tExpr2)

instance Lua Value where
  toLua (Number n) = show n
  toLua (Character c) = ['\'', c, '\'']
  toLua (Atom n) = toUpper <$> n
  toLua (List t []) = "{}"
  toLua (List t (x:xs)) = concat ["{ head = ", toLua x, ", tail = ", toLua (List t xs), " }"]
  toLua (Tuple e1 e2)
    =  toLua <$> [e1, e2]
    &  showTuple

instance Lua Expr where
  toLua (Val p) = toLua p
  toLua (Ident name) = name
  toLua (Call name exprs) = name ++
    case exprs of
      [] -> ""  -- functions without arguments are interpreted as values
      _ -> concatMap (paren . toLua) exprs
  toLua (UnOp unop e) = toLua e ++ toLua unop
  toLua (BinOp bop e1 e2) = toLua $ Call (toLua bop) [e1, e2]
  toLua (TernOp If p e1 e2) = (paren . unwords) [toLua p, "> 0 and", toLua e1, "or", toLua e2] -- this works b/c all ints are truthy in lua
  toLua (TernOp Uncons xs b fb) =
    toLua $ TernOp If (BinOp Equal xs (Val $ List (Unspecified "") [])) b (Call (toLua fb) [UnOp Head xs, UnOp Tail xs])

instance Lua UnOp where
  toLua Fst = "[1]"
  toLua Snd = "[2]"
  toLua Head = ".head"
  toLua Tail = ".tail"

instance Lua Bop where
  toLua Plus = "_plus_"
  toLua Minus = "_minus_"
  toLua Times = "_mult_"
  toLua Divide = "_div_"
  toLua GreaterThan = "_gt_"
  toLua GreaterThanOrEqual = "_gte_"
  toLua LessThan = "_lt_"
  toLua LessThanOrEqual = "_lte_"
  toLua Rem = "_rem_"
  toLua Equal = "_eq_"
  toLua Cons = "_cons_"

showAtoms :: [Function] -> [String]
showAtoms funcs
  =  concatMap (findAtoms . body) funcs
  &  zip [0..] . nub . ("False" :) . ("True" :)
 <&> (\(i, atom) -> unwords [toUpper <$> atom, "=", show i] ++ ";")

showZeroArityFunctions :: [Function] -> [String]
showZeroArityFunctions funcs
  =  funcs
  &  filter (null . args)
 <&> name
 <&> (\name -> unwords [name, "=", "__" ++ name ++ "()" ])

showTopology :: Map Name Stream -> [Pipe] -> String
showTopology _ [] = "pipes = {}"
showTopology streamMap pipes
  =  pipes
 <&> showPipe streamMap
  &  ("pipes = " ++) . ("{\n" ++) . (++ "\n}") . indent . intercalate ",\n"

showPipe :: Map Name Stream -> Pipe -> String
showPipe streamMap Pipe { funcName, inStreams, outStreamName } =
  showTuple [funcName, (curly . intercalate ", ") (fmap (\(name, buffer) -> showTuple [show name, show buffer]) inStreams), show outStreamName]

showTuple :: [String] -> String
showTuple = curly . intercalate ", "
