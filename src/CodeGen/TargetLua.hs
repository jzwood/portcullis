{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.TargetLua where

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
        body = toLambda vars expr
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
    &  bracket . unwords
  toLua (Arrow tExpr1 tExpr2) = paren (toLua tExpr1 ++ pad "->" ++ toLua tExpr2)

instance Lua Value where
  toLua (Number n) = show n
  toLua (Character c) = ['\'', c, '\'']
  toLua (Atom n) = toUpper <$> n
  toLua (List t xs) = showList $ toLua <$> xs
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
  toLua (BinOp Equal e1 e2) = "int" ++ infixBop Equal e1 e2
  toLua (BinOp Cons e xs) = unwords [ bracket $ toLua e, "+", toLua xs]
  toLua (BinOp bop e1 e2) = infixBop bop e1 e2
  toLua (TernOp If p e1 e2) = (paren . unwords) [ toLua e1, "if", toLua p, "else", toLua e2]
  toLua (TernOp Uncons xs b fb) =
    toLua $ TernOp If (BinOp Equal xs (Val $ List (Unspecified "") [])) b (Call (toLua fb) [UnOp Head xs, UnOp Tail xs])

instance Lua UnOp where
  toLua Fst = "[1]"
  toLua Snd = "[2]"
  toLua Head = ".head"  -- not a Lua thing. this is a Portcullis convention
  toLua Tail = ".tail"  -- ditto above

instance Lua Bop where
  toLua Plus = "+"
  toLua Minus = "-"
  toLua Times = "*"
  toLua Divide = "/"
  toLua GreaterThan = ">"
  toLua GreaterThanOrEqual = ">="
  toLua LessThan = "<"
  toLua LessThanOrEqual = "<="
  toLua Rem = "%"
  toLua Equal = "=="
  --show Cons = "+>"  -- not used for code gen

prefixOp :: String -> [String] -> String
prefixOp op = (op ++) . paren . intercalate ", "

infixBop :: Bop -> Expr -> Expr -> String
infixBop bop e1 e2 = paren . intercalate (pad . toLua $ bop) $ toLua <$> [e1, e2]

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
