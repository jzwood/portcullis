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
    = unlines $ atoms : zeroArityFuncs : (show <$> functions) ++ [topology]
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
        docstring = comment $ unwords ["signature:", show tExpr]
        header = concat [ if null vars then "function $" else "export function " , name , (paren . head' "") vars ]
        body = (indent . concat) [ "return " , concatMap ((++ " => ") . paren) (tail' vars) , show expr , ";" ]

instance Js Comment where
  toJs (Comment c) = comment c

instance Js TypeExpr where
  toJs NumType = "Num"
  toJs CharType = "Char"
  toJs AtomType = "Atom"
  toJs (Unspecified t) = t
  toJs (ListType t)
    = show t
    & bracket
  toJs (TupType t1 t2)
    =  [t1, t2]
   <&> show
    &  bracket . unwords
  toJs (Arrow tExpr1 tExpr2) = paren (show tExpr1 ++ pad "->" ++ show tExpr2)

instance Js Value where
  toJs (Number n) = show n
  toJs (Character c) = ['\'', c, '\'']
  toJs (Atom n) = n
  toJs (List (Unspecified "") xs) = show xs -- so uncons displays nicely
  toJs (List t xs) = unwords ["/*", show $ ListType t, "*/", show xs]
  toJs (Tuple e1 e2)
    =  show <$> [e1, e2]
    &  showList

instance Js Expr where
  toJs (Val p) = show p
  toJs (Ident name) = name
  toJs (Call name exprs) = name ++
    case exprs of
      [] -> ""  -- functions without arguments are interpreted as values
      _ -> concatMap (paren . show) exprs
  toJs (UnOp unop e) = show e ++ show unop
  toJs (BinOp Equal e1 e2) = prefixBop Equal e1 e2
  toJs (BinOp Cons e xs) = bracket $ show e ++ ", ..." ++ show xs
  toJs (BinOp bop e1 e2) = infixBop bop e1 e2
  toJs (TernOp If p e1 e2)
    = (paren . ('\n':) . (++"\n") . indent . unlines)
    [ "/* if */ " ++ show p ++ " ?"
    , "/* then */ " ++ show e1 ++ " :"
    , "/* else */ " ++ show e2
    ]
  show (TernOp Uncons xs b fb) =
    show $ TernOp If (BinOp Equal xs (Val $ List (Unspecified "") [])) b (Call (show fb) [UnOp Head xs, UnOp Tail xs])

instance Show UnOp where
  show Fst = "[0]"
  show Snd = "[1]"
  show Head = ".at(0)"
  show Tail = ".slice(1)"

instance Show Bop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show Rem = "%"
  show Equal = "equal"
  --show Cons = "+>"  -- not used for code gen

prefixOp :: String -> [String] -> String
prefixOp op = (op ++) . paren . intercalate ", "

prefixBop :: Bop -> Expr -> Expr -> String
prefixBop bop e1 e2 = prefixOp (show bop) (show <$> [e1, e2])

infixBop :: Bop -> Expr -> Expr -> String
infixBop bop e1 e2 = paren . intercalate (pad . show $ bop) $ show <$> [e1, e2]

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
