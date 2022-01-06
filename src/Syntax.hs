module Syntax where

import Data.Functor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, nub)
import Util

-- Notes:
-- no Var (variables): instead you have functions with zero parameters
-- no Extern (external): we're just not gonna let you call external functions :/
-- TODO steal from this: https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

type Name = String

--data Stmt
  -- = Data Type
  -- | Function Name [Var] Expr
  -- | FunType Name [Type]
  -- | Pipe Name [Name] [Name] -- pipe name, input function names, output function names

data Module = Module [Stmt]

data Pipeline
  = InOut Name

data Stmt = Function
  { name :: Name
  , signature :: TypeExpr
  , args :: [Name]
  , body :: Expr
  } deriving (Eq)

data TypeExpr
  = NumType
  | CharType
  | AtomType
  | Unspecfied Name
  | TupType TypeExpr TypeExpr
  | ListType TypeExpr
  | Arrow TypeExpr TypeExpr
  deriving (Eq)

data Value
  = Number Double -- 34.23
  | Character Char -- 'b'
  | Atom Name -- Apple
  | Tuple Expr Expr --- [1 'a']
  | List TypeExpr [Expr] --- num [1, 2, 3]
  deriving (Eq)

data Expr
  = Val Value
  | Ident Name  -- arg
  | Call Name [Expr]  -- add 12 45 (function invocation)
  | Guard [(Expr, Expr)] Expr
  | UnOp UnOp Expr
  | BinOp Bop Expr Expr  -- + 2 3
  | TernOp Top Expr Expr Expr
  deriving (Eq)

data UnOp
  = Fst
  | Snd
  deriving (Eq)

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | GreaterThan
  | GreaterThanOrEqual
  | LessThan
  | LessThanOrEqual
  | Equal
  | Rem
  | Concat
  deriving (Eq)

data Top
  = Slice
  | At
  deriving (Eq)
