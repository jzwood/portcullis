{-# LANGUAGE NamedFieldPuns #-}

module CodeGen where

import Syntax
import Data.Functor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, intersperse, nub)
import Util

instance Show Module where
  show (Module stmts)
    = unlines $ atoms : zeroArityFuncs : (show <$> stmts)
      where
        funcs = foldr filterFuncAlg [] stmts
        atoms = readAtoms funcs
        zeroArityFuncs = readZeroArityFunctions funcs

filterFuncAlg :: Stmt -> [Func] -> [Func]
filterFuncAlg (F func) funcs = func : funcs
filterFuncAlg _ funcs = funcs

instance Show Stmt where
  show (F func) = show func
  show (C comment) = show comment
  show (Q queue) = "" -- queues have no explicit code output
  show (P pipe) = show pipe

instance Show Func where
  show (Function name tExpr vars expr)
    = comment $ unwords [show name, "::", show tExpr]
    ++ '\n'
    : concat
    [ def , name , (paren . head' "") vars ]
    ++ unlines'
    [ " {"
    , (indent . concat) [ "return " , concatMap ((++ " => ") . paren) (tail' vars) , show expr , ";" ]
    , "}"
    ]
      where
        def = case vars of
          []  -> "function $"
          _ -> "export function "

instance Show Comment where
  show (Comment comment) = unwords ["/*", comment, "*/"]

instance Show Pipe where
  show (Pipe Function { name } ins out) = prefixOp "addPipe" [show $ queueName <$> ins, queueName out]

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
    &  bracket . intercalate ", "

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

readAtoms :: [Func] -> String
readAtoms funcs
  =  concatMap (findAtoms . body) funcs
  &  zip [0..] . nub . ("False" :) . ("True" :)
 <&> (\(i, atom) -> unwords ["const", atom, "=", show i])
  &  unlines

readZeroArityFunctions :: [Func] -> String
readZeroArityFunctions funcs
  =  funcs
  &  filter (null . args)
 <&> name
 <&> (\name -> unwords ["export", "const", name, "=", '$' : name ++ "()" ])
  &  unlines
