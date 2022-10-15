{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.TargetPy where

import Prelude hiding (showList)
import Syntax
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

instance Py Comment where
  toPy (Comment c) = ""  -- python doesn't have inline comments

instance Py TypeExpr where
  toPy NumType = "Num"
  toPy CharType = "Char"
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
  toPy (Character c) = ['\'', c, '\'']
  toPy (Atom n) = n
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
  toPy (BinOp Cons e xs) = unwords [ bracket $ toPy e, "+", toPy xs]
  toPy (BinOp bop e1 e2) = infixBop bop e1 e2
  toPy (TernOp If p e1 e2) = unwords [ toPy e1, "if", toPy p, "else", toPy e2]
  toPy (TernOp Uncons xs b fb) =
    toPy $ TernOp If (BinOp Equal xs (Val $ List (Unspecified "") [])) b (Call (toPy fb) [UnOp Head xs, UnOp Tail xs])

instance Py UnOp where
  toPy Fst = "[0]"
  toPy Snd = "[1]"
  toPy Head = "[0]"
  toPy Tail = "[1:]"

instance Py Bop where
  toPy Plus = "+"
  toPy Minus = "-"
  toPy Times = "*"
  toPy Divide = "/"
  toPy GreaterThan = ">"
  toPy GreaterThanOrEqual = ">="
  toPy LessThan = "<"
  toPy LessThanOrEqual = "<="
  toPy Rem = "%"
  toPy Equal = "=="
  --show Cons = "+>"  -- not used for code gen

prefixOp :: String -> [String] -> String
prefixOp op = (op ++) . paren . intercalate ", "

infixBop :: Bop -> Expr -> Expr -> String
infixBop bop e1 e2 = paren . intercalate (pad . toPy $ bop) $ toPy <$> [e1, e2]

flatFindAtoms :: [Expr] -> [String]
flatFindAtoms = concatMap findAtoms

findAtoms :: Expr -> [String]
findAtoms (Val v) =
  case v of
    Atom n -> [n]
    Tuple e1 e2 -> flatFindAtoms [e1, e2]
    List _ es -> flatFindAtoms es
    _ -> []
findAtoms (Call n [es]) = flatFindAtoms [es]
findAtoms (BinOp _ e1 e2) = flatFindAtoms [e1, e2]
findAtoms (TernOp _ e1 e2 e3) = flatFindAtoms [e1, e2, e3]
findAtoms _ = []

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
