module Syntax where

import Data.Functor
import Control.Applicative
import Data.Char
import Data.List (intercalate)
import Util

-- Notes:
-- no Var (variables): instead you have functions with zero parameters
-- no Extern (external): we're just not gonna let you call external functions :/
-- TODO steal from this: https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

type Name = String

newtype Var = Var String
  deriving (Eq)

instance Show Var where
  show (Var str) = str

--data Stmt
  -- = Data Type
  -- | Function Name [Var] Expr
  -- | FunType Name [Type]
  -- | Pipe Name [Name] [Name] -- pipe name, input function names, output function names

data Module = Module Name [Stmt]

data Pipeline
  = InOut Name

data Stmt  -- aka Declaration
  = Signature Name TypeExpr
  | Function Name [Var] Expr
  deriving (Eq)

data Statement = Statement
  { funcName :: Name
  , signature :: TypeExpr
  , args :: [Var]
  , body :: Expr
  } deriving (Show, Eq)

-- data ConcreteType = NumType | CharType | AtomType

data TypeExpr
  = NumType
  | CharType
  | AtomType
  | Unspecfied Name
  | ListType TypeExpr
  | Arrow TypeExpr TypeExpr
  deriving (Eq)

data Value
  = Number Double -- 34.23
  | Character Char -- 'b'
  | Atom Name -- Apple
  | List TypeExpr [Expr] --- num [1, 2, 3]
  deriving (Eq)

data Expr
  = Val Value
  | Ident Name  -- arg
  | Call Name [Expr]  -- add 12 45 (function invocation)
  | Guard [(Expr, Expr)]
  | BinOp Bop Expr Expr  -- + 2 3
  | TernOp Top Expr Expr Expr
  deriving (Eq)

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | GreaterThan
  | LessThan
  | Equal
  | NotEqual
  | Mod
  | Concat
  deriving (Eq)

data Top
  = Fold
  | Unfold
  deriving (Eq)

instance Show TypeExpr where
  show NumType = "Num"
  show CharType = "Char"
  show AtomType = "Atom"
  show (Unspecfied t) = t
  show (ListType t) = '[' : (show t) ++ "]"
  show (Arrow tExpr1 tExpr2) = paren (show tExpr1 ++ (" -> ") ++ show tExpr2)

instance Show Stmt where
  show (Signature name tExpr) = comment $ concat ["function ", show name, " has type ", show tExpr]
  show (Function name vars expr) = concat
    [ "function "
    , name
    , paren (intercalate ", " (show <$> vars))
    , concat [" {\n", indent ("return " ++ (show expr) ++ ";"), "}"]
    ]

showGuardCase :: (Expr, Expr) -> String
showGuardCase (expr1, expr2) = "\n\tif (" ++ (show expr1) ++ ") {\n\t\treturn " ++ (show expr2) ++ ";\n\t}"

instance Show Value where
  show (Number n) = show n
  show (Character c) = '\'' : c : '\'' : []
  show (Atom n) = n
  show (List t a) = intercalate " " ["/*", show $ ListType t, "*/", show a]

instance Show Expr where
  show (Val p) = show p
  show (Ident name) = name
  show (Call name exprs) = name ++ paren (intercalate ", " $ show <$> exprs)
  show (BinOp bop expr1 expr2) = paren . concat $ [paren . show $ expr1, show bop, paren . show $ expr2]
  show (Guard exprExprs) = concat ["(() => {", (intercalate " " $ showGuardCase <$> exprExprs), "\n})()"]
  show (TernOp top expr1 expr2 expr3) = show top ++ paren (intercalate ", " $ show <$> [expr1, expr2, expr3])

instance Show Bop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"
  show GreaterThan = ">"
  show LessThan = "<"
  show Equal = "==="
  show NotEqual = "!=="
  show Mod = "%"
  show Concat = ".concat"

instance Show Top where
  show Fold = "fold"
  show Unfold = "unfold"
