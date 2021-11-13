module Syntax where

import MiniParser
import Data.Functor
import Control.Applicative
import Data.Char
import Data.List
import Util

-- Notes:
-- no Var (variables): instead you have functions with zero parameters
-- no Extern (external): we're just not gonna let you call external functions :/
-- TODO steal from this: https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

type Name = String

newtype Var = Var String
  deriving (Show, Eq)

--data Stmt
  -- = Data Type
  -- | Function Name [Var] Expr
  -- | FunType Name [Type]
  -- | Pipe Name [Name] [Name] -- pipe name, input function names, output function names

-- NonZero = Real x where x /= 0

type Mod = [Stmt]

data Pipeline
  = InOut Name

data Stmt
  = Type Name PredicateExpr
  | Signature Name TypeExpr
  | Function Name [Var] Expr
  deriving (Show, Eq)

data PredicateExpr
  = X
  | Real Double
  | Binomial Bop PredicateExpr PredicateExpr
  deriving (Eq, Show)

data TypeExpr
  = NumType String
  | Arrow TypeExpr TypeExpr
  deriving (Show, Eq)

data Expr
  = Number Double -- 34.23
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
  | And
  | Or
  | Mod
  deriving (Eq)

data Top
  = Fold
  | Unfold
  deriving (Eq, Show)

showGuardCase :: (Expr, Expr) -> String
showGuardCase (expr1, expr2) = "\n\tif (" ++ (show expr1) ++ ") {\n\t\treturn " ++ (show expr2) ++ ";\n\t}"

instance Show Expr where
  show (Number n) = show n
  show (Ident name) = name
  show (Call name exprs) = name ++ parenthize (intercalate ", " $ show <$> exprs)
  show (BinOp bop expr1 expr2) = parenthize (show expr1 ++ (pad $ show bop) ++ show expr2)
  show (Guard exprExprs) = concat ["(() => {", (intercalate " " $ showGuardCase <$> exprExprs), "\n})()"]

instance Show Bop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"
  show GreaterThan = ">"
  show LessThan = "<"
  show Equal = "==="
  show NotEqual = "!=="
  show And = "&&"
  show Or = "||"
  show Mod = "%"
