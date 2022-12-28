{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.TargetJs where

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

class Js ast where
  toJs :: ast -> String

instance Js Module where
  toJs Module { functions, comments, streamMap, pipes }
    = unlines $ (toJs <$> functions) ++ [atoms, zeroArityFuncs, topology]
      where
        atoms = unlines $ showAtoms functions
        zeroArityFuncs = unlines $ showZeroArityFunctions functions
        topology = showTopology streamMap pipes

instance Js Function where
  toJs (Function name tExpr vars expr)
    = unlines
    [ docstring
    , unwords [header, "{"]
    , body
    , "}"
    ]
      where
        docstring = unwords ["// signature:", toJs tExpr]
        header = concat [ if null vars then "function $" else "export function " , name , (paren . head' "") vars ]
        body = (indent . concat) [ "return " , concatMap ((++ " => ") . paren) (tail' vars) , toJs expr , ";" ]

instance Js TypeExpr where
  toJs NumType = "Num"
  toJs CharType = "Char"
  toJs AtomType = "Atom"
  toJs (Unspecified t) = t
  toJs (ListType t)
    = toJs t
    & bracket
  toJs (TupType t1 t2)
    =  [t1, t2]
   <&> toJs
    &  bracket . unwords
  toJs (Arrow tExpr1 tExpr2) = paren (toJs tExpr1 ++ pad "->" ++ toJs tExpr2)

instance Js Value where
  toJs (Number n) = show n
  toJs (Character c) = ['\'', c, '\'']
  toJs (Atom n) = n
  toJs (List (Unspecified "") xs) = showList $ toJs <$> xs -- so uncons displays nicely
  toJs (List t xs) = unwords ["/*", toJs $ ListType t, "*/", showList $ toJs <$> xs]
  toJs (Tuple e1 e2)
    =  toJs <$> [e1, e2]
    &  showList

instance Js Expr where
  toJs (Val p) = toJs p
  toJs (Ident name) = name
  toJs (Call name exprs) = name ++
    case exprs of
      [] -> ""  -- functions without arguments are interpreted as values
      _ -> concatMap (paren . toJs) exprs
  toJs (UnOp unop e) = toJs e ++ toJs unop
  toJs (BinOp bop e1 e2) = toJs $ Call (toJs bop) [e1, e2]
  toJs (TernOp If p e1 e2)
    = (paren . ('\n':) . (++"\n") . indent . unwords)
    [ toJs p ++ " ?"
    , toJs e1 ++ " :"
    , toJs e2
    ]
  toJs (TernOp Uncons xs b fb) =
    toJs $ TernOp If (BinOp Equal xs (Val $ List (Unspecified "") [])) b (Call (toJs fb) [UnOp Head xs, UnOp Tail xs])

instance Js UnOp where
  toJs Fst = "[0]"
  toJs Snd = "[1]"
  toJs Head = ".at(0)"
  toJs Tail = ".slice(1)"

instance Js Bop where
  toJs Plus = "_plus_"
  toJs Minus = "_minus_"
  toJs Times = "_mult_"
  toJs Divide = "_div_"
  toJs GreaterThan = "_gt_"
  toJs GreaterThanOrEqual = "_gte_"
  toJs LessThan = "_lt_"
  toJs LessThanOrEqual = "_lte_"
  toJs Rem = "_rem_"
  toJs Equal = "_eq_"
  toJs Cons = "_cons_"

prefixOp :: String -> [String] -> String
prefixOp op = (op ++) . paren . intercalate ", "

prefixBop :: Bop -> Expr -> Expr -> String
prefixBop bop e1 e2 = prefixOp (toJs bop) (toJs <$> [e1, e2])

infixBop :: Bop -> Expr -> Expr -> String
infixBop bop e1 e2 = paren . intercalate (pad . toJs $ bop) $ toJs <$> [e1, e2]

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
