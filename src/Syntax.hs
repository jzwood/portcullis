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

data Module = Module { functions :: [Function], functionMap :: Map Name Function, comments :: [Comment], queues :: [Queue], queueMap :: Map Name Queue, pipes :: [Pipe] }

data Queue = Queue { queueName :: Name, buffer :: Integer, queueSig :: TypeExpr }
  deriving (Eq, Ord)
data Pipe = Pipe { funcName :: Name, inQueueNames :: [Name], outQueueName :: Name }
  deriving (Eq, Ord, Show)
newtype Comment = Comment String
  deriving (Eq, Ord)

data Function = Function
  { name :: Name
  , signature :: TypeExpr
  , args :: [Name]
  , body :: Expr
  } deriving (Eq, Ord)

data Stmt = F Function | Q Queue | P Pipe | C Comment deriving (Eq)

data TypeExpr
  = NumType
  | CharType
  | AtomType
  | Unspecfied Name
  | TupType TypeExpr TypeExpr
  | ListType TypeExpr
  | Arrow TypeExpr TypeExpr
  deriving (Eq, Ord)

data Value
  = Number Double -- 34.23
  | Character Char -- 'b'
  | Atom Name -- Apple
  | Tuple Expr Expr --- [1 'a']
  | List TypeExpr [Expr] --- num [1, 2, 3]
  deriving (Eq, Ord)

data Expr
  = Val Value
  | Ident Name  -- arg
  | Call Name [Expr]  -- add 12 45 (function invocation)
  | UnOp UnOp Expr
  | BinOp Bop Expr Expr  -- + 2 3
  | TernOp Top Expr Expr Expr
  deriving (Eq, Ord)

data UnOp
  = Fst
  | Snd
  | Head -- INTERNAL ONLY
  | Tail  -- INTERNAL ONLY
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

data Top
  = Uncons
  | If
  deriving (Show, Eq, Ord)
