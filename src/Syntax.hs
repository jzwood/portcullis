module Syntax where

import Data.Functor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Util

type Name = String

data Module = Module { functions :: [Function], functionMap :: Map Name Function, comments :: [Comment], queueMap :: Map Name Queue, pipes :: [Pipe] }

data Queue = Queue { queueName :: Name, buffer :: Integer, queueSig :: TypeExpr }
  deriving (Eq)
data Pipe = Pipe { funcName :: Name, inQueueNames :: [Name], outQueueName :: Name }
  deriving (Eq, Show)
newtype Comment = Comment String
  deriving (Eq)

data Function = Function
  { name :: Name
  , signature :: TypeExpr
  , args :: [Name]
  , body :: Expr
  } deriving (Eq)

data Stmt = F Function | Q Queue | P Pipe | C Comment deriving (Eq)

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
  | UnOp UnOp Expr
  | BinOp Bop Expr Expr  -- + 2 3
  | TernOp Top Expr Expr Expr
  deriving (Eq)

data UnOp
  = Fst
  | Snd
  | Head -- INTERNAL ONLY
  | Tail  -- INTERNAL ONLY
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
  | Cons
  deriving (Eq)

data Top
  = Uncons
  | If
  deriving (Show, Eq)
