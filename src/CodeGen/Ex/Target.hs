{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.Ex.Target where

import Prelude hiding (showList)
import Syntax
import Data.Functor
import Data.Bifunctor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, intersperse, nub)
import Utils
import Data.Map (Map, (!))
import qualified Data.Map as Map

pathToModuleName :: String -> String
pathToModuleName "" = ""
pathToModuleName (h:t) = toUpper h : filter isAlphaNum t

class Ex ast where
  toEx :: ast -> String

instance Ex Module where
  toEx Module { functions, comments, streamMap, pipes }
    = unlines $ (toEx <$> functions) ++ [zeroArityFuncs, topology]
      where
        zeroArityFuncs = unlines $ showZeroArityFunctions functions
        topology = showTopology streamMap pipes

instance Ex Function where
  toEx (Function name tExpr vars expr)
    = unlines [ docstring , header ++ body ++ "." ]
      where
        docstring = unwords ["%% signature:", toEx tExpr]
        header = concat [ if null vars then "$" else "", name , (paren . head' "") vars, " ->" ]
        body = (indent . concat) [ "fun" , concatMap ((++ " -> ") . paren) (tail' vars) , toEx expr , " end " ]

instance Ex TypeExpr where
  toEx NumType = "Num"
  toEx ByteType = "Byte"
  toEx AtomType = "Atom"
  toEx (Unspecified t) = t
  toEx (ListType t)
    = toEx t
    & bracket
  toEx (TupType t1 t2)
    =  [t1, t2]
   <&> toEx
    &  bracket . unwords
  toEx (Arrow tExpr1 tExpr2) = paren (toEx tExpr1 ++ pad "->" ++ toEx tExpr2)

instance Ex Value where
  toEx (Number n) = show n
  toEx (Byte b) = show b
  toEx (Atom n) = n
  toEx (List (Unspecified "") xs) = showList $ toEx <$> xs -- so uncons displays nicely
  toEx (List t xs) = unwords [showList $ toEx <$> xs]
  toEx (Tuple e1 e2)
    =  toEx <$> [e1, e2]
    &  showList

instance Ex Expr where
  toEx (Val p) = toEx p
  toEx (Ident name) = name
  toEx (Call name exprs) = name ++
    case exprs of
      [] -> ""  -- functions without arguments are interpreted as values
      _ -> paren (concatMap (paren . toEx) exprs)
  toEx (UnOp unop e) = toEx e ++ toEx unop
  toEx (BinOp bop e1 e2) = toEx $ Call (toEx bop) [e1, e2]
  toEx (TernOp top e1 e2 e3) = toEx $ Call (toEx top) [e1, e2, e3]

instance Ex UnOp where
  toEx Fst = "[0]"
  toEx Snd = "[1]"
  toEx Head = ".at(0)"
  toEx Tail = ".slice(1)"

instance Ex Bop where
  toEx Plus = "plus_"
  toEx Minus = "minus_"
  toEx Times = "mult_"
  toEx Divide = "div_"
  toEx GreaterThan = "gt_"
  toEx GreaterThanOrEqual = "gte_"
  toEx LessThan = "lt_"
  toEx LessThanOrEqual = "lte_"
  toEx Rem = "rem_"
  toEx Equal = "eq_"
  toEx Cons = "cons_"

instance Ex Top where
  toEx If = "if_"
  toEx Uncons = "uncons_"

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
