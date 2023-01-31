{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.Py.TargetPy where

import Prelude hiding (showList)
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

class Py ast where
  toPy :: ast -> String

instance Py Module where
  toPy Module { functions, comments, streamMap, pipes }
    = unlines $ (toPy <$> functions) ++ [atoms, zeroArityFuncs, topology]
      where
        atoms = unlines $ showAtoms functions
        zeroArityFuncs = unlines $ showZeroArityFunctions functions
        topology = showTopology streamMap pipes

instance Py Function where
  toPy (Function name tExpr vars expr)
    = unlines [ docstring , header, body ]
      where
        docstring = unwords ["# signature:", toPy tExpr]
        header = concat [ if null vars then "def __" else "def " , name, (paren . head' "") vars, ":" ]
        body = (indent . concat) [ "return " , concatMap (("lambda " ++) . (++ ": ")) (tail' vars) , toPy expr]

instance Py TypeExpr where
  toPy NumType = "Num"
  toPy ByteType = "Byte"
  toPy AtomType = "Atom"
  toPy (Unspecified t) = t
  toPy (ListType t)
    = toPy t
    & bracket
  toPy (TupType t1 t2)
    =  [t1, t2]
   <&> toPy
    &  bracket . unwords
  toPy (Arrow tExpr1 tExpr2) = paren (toPy tExpr1 ++ pad "->" ++ toPy tExpr2)

instance Py Value where
  toPy (Number n) = show n
  toPy (Byte b) = show b
  toPy (Atom n) = toUpper <$> n
  toPy (List t xs) = showList $ toPy <$> xs
  toPy (Tuple e1 e2)
    =  toPy <$> [e1, e2]
    &  showTuple

instance Py Expr where
  toPy (Val p) = toPy p
  toPy (Ident name) = name
  toPy (Call name exprs) = name ++
    case exprs of
      [] -> ""  -- functions without arguments are interpreted as values
      _ -> concatMap (paren . toPy) exprs
  toPy (UnOp unop e) = toPy e ++ toPy unop
  toPy (BinOp bop e1 e2) = toPy $ Call (toPy bop) [e1, e2]
  toPy (TernOp If p e1 e2) = (paren . unwords) [ toPy e1, "if", toPy p, "else", toPy e2]
  toPy (TernOp Uncons xs b fb) =
    toPy $ TernOp If (BinOp Equal xs (Val $ List (Unspecified "") [])) b (Call (toPy fb) [UnOp Head xs, UnOp Tail xs])

instance Py UnOp where
  toPy Fst = "[0]"
  toPy Snd = "[1]"
  toPy Head = "[0]"
  toPy Tail = "[1:]"

instance Py Bop where
  toPy Plus = "_plus_"
  toPy Minus = "_minus_"
  toPy Times = "_mult_"
  toPy Divide = "_div_"
  toPy GreaterThan = "_gt_"
  toPy GreaterThanOrEqual = "_gte_"
  toPy LessThan = "_lt_"
  toPy LessThanOrEqual = "_lte_"
  toPy Rem = "_rem_"
  toPy Equal = "_eq_"
  toPy Cons = "_cons_"

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
showTopology _ [] = "pipes = [];"
showTopology streamMap pipes
  =  pipes
 <&> showPipe streamMap
  &  ("pipes = " ++) . ("[\n" ++) . (++ "\n]") . indent . intercalate ",\n"

showPipe :: Map Name Stream -> Pipe -> String
showPipe streamMap Pipe { funcName, inStreams, outStreamName } =
  showTuple [funcName, showList (fmap (\(name, buffer) -> showTuple [show name, show buffer]) inStreams), show outStreamName]

showTuple :: [String] -> String
showTuple = paren . intercalate ", "
