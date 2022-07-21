{-# LANGUAGE NamedFieldPuns #-}

module CodeGen where

import Prelude hiding (showList)
import Syntax
import Data.Functor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, intersperse, nub)
import Util
import Data.Map (Map, (!))
import qualified Data.Map as Map

instance Show Module where
  show Module { functions, comments, queueMap, pipes }
    = unlines $ atoms : zeroArityFuncs : (show <$> functions) ++ [topology]
      where
        atoms = unlines $ showAtoms functions
        zeroArityFuncs = unlines $ showZeroArityFunctions functions
        topology = showTopology queueMap pipes

instance Show Function where
  show (Function name tExpr vars expr)
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

instance Show Comment where
  show (Comment c) = comment c

instance Show TypeExpr where
  show NumType = "Num"
  show CharType = "Char"
  show AtomType = "Atom"
  show (Unspecfied t) = t
  show (ListType t)
    = show t
    & bracket
  show (TupType t1 t2)
    =  [t1, t2]
   <&> show
    &  bracket . unwords
  show (Arrow tExpr1 tExpr2) = paren (show tExpr1 ++ pad "->" ++ show tExpr2)

instance Show Value where
  show (Number n) = show n
  show (Character c) = ['\'', c, '\'']
  show (Atom n) = n
  show (List (Unspecfied "") xs) = show xs -- so uncons displays nicely
  show (List t xs) = unwords ["/*", show $ ListType t, "*/", show xs]
  show (Tuple e1 e2)
    =  show <$> [e1, e2]
    &  showList

instance Show Expr where
  show (Val p) = show p
  show (Ident name) = name
  show (Call name exprs) = name ++
    case exprs of
      [] -> ""  -- functions without arguments are interpreted as values
      _ -> concatMap (paren . show) exprs
  show (UnOp unop e) = show e ++ show unop
  show (BinOp Equal e1 e2) = prefixBop Equal e1 e2
  show (BinOp Cons e xs) = bracket $ show e ++ ", ..." ++ show xs
  show (BinOp bop e1 e2) = infixBop bop e1 e2
  show (TernOp If p e1 e2)
    = (paren . ('\n':) . (++"\n") . indent . unlines)
    [ "/* if */ " ++ show p ++ " ?"
    , "/* then */ " ++ show e1 ++ " :"
    , "/* else */ " ++ show e2
    ]
  show (TernOp Uncons xs b fb) =
    show $ TernOp If (BinOp Equal xs (Val $ List (Unspecfied "") [])) b (Call (show fb) [UnOp Head xs, UnOp Tail xs])

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
 <&> (\(i, atom) -> unwords ["const", atom, "=", show i])

showZeroArityFunctions :: [Function] -> [String]
showZeroArityFunctions funcs
  =  funcs
  &  filter (null . args)
 <&> name
 <&> (\name -> unwords ["export", "const", name, "=", '$' : name ++ "()" ])

showTopology :: Map Name Queue -> [Pipe] -> String
showTopology _ [] = "export function exportGraph()" ++ (curly . indent) "return [];"
showTopology queueMap pipes
  =  pipes
 <&> showPipe queueMap
  &  ("export const topology = " ++) . ("[\n" ++) . (++ "\n]") . indent . intercalate ",\n"

showPipe :: Map Name Queue -> Pipe -> String
showPipe queueMap Pipe { funcName, inQueueNames, outQueueName } =
  showList [funcName, inQueuesNamesBuffers, show outQueueName]
  where
    inQueuesNamesBuffers = showList ((\name -> showList [show name, (show . buffer) (queueMap ! name)]) <$> inQueueNames)
