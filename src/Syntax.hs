module Syntax where

import Data.Functor
import Data.Function
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
  | Guard [(Expr, Expr)]
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
    &  intercalate " "
    &  bracket
  show (Arrow tExpr1 tExpr2) = paren (show tExpr1 ++ (pad "->") ++ show tExpr2)

instance Show Stmt where
  show (Signature name tExpr) = comment $ concat ["function ", show name, " has type ", show tExpr]
  show (Function name vars expr) = concat
    [ "function "
    , name
    , paren (intercalate ", " (show <$> vars))
    , concat [" {\n", indent ("return " ++ (show expr) ++ ";"), "}"]
    ]

showGuardCase :: (Expr, Expr) -> String
showGuardCase (expr1, expr2) = concat ["\n\tif (", show expr1, ") {\n\t\treturn ", show expr2,  ";\n\t}"]

instance Show Value where
  show (Number n) = show n
  show (Character c) = '\'' : c : '\'' : []
  show (Atom n) = n
  show (List t a) = intercalate " " ["/*", show $ ListType t, "*/", show a]
  show (Tuple e1 e2)
    =  show <$> [e1, e2]
    &  bracket . (intercalate ",")

prefixOp :: String -> [String] -> String
prefixOp op = (op ++) . paren . (intercalate ", ")

prefixBop :: Bop -> Expr -> Expr -> String
prefixBop bop e1 e2 = prefixOp (show bop) (show <$> [e1, e2])

prefixTop :: Top -> Expr -> Expr -> Expr -> String
prefixTop top e1 e2 e3 = prefixOp (show top) (show <$> [e1, e2, e3])

infixBop :: Bop -> Expr -> Expr -> String
infixBop bop e1 e2 = paren . (intercalate $ show bop) $ show <$> [e1, e2]

instance Show Expr where
  show (Val p) = show p
  show (Ident name) = name
  show (Call name exprs) = name ++ paren (intercalate ", " $ show <$> exprs)
  show (BinOp Equal e1 e2) = prefixBop Equal e1 e2
  show (BinOp Concat a1 a2) = prefixBop Concat a1 a2
  show (BinOp bop e1 e2) = infixBop bop e1 e2
  show (Guard exprExprs) = concat ["(() => {", (intercalate " " $ showGuardCase <$> exprExprs), "\n})()"]
  show (TernOp At a n e) = prefixOp (show At) (show <$> [a, n]) ++ " ?? " ++ show n
  show (TernOp top e1 e2 e3) = prefixTop top e1 e2 e3

instance Show UnOp where
  show Fst = "(([a,]) => a)"
  show Snd = "(([,b]) => b)"

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
  show Concat = "Array.prototype.concat.call"

instance Show Top where
  show Slice = "Array.prototype.slice.call"
  show At = "Array.prototype.at.call"
