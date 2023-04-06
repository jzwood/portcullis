{-# LANGUAGE StrictData #-}

module Syntax where

import Data.Functor
import Data.Function
import Control.Applicative
import Data.Map (Map)
--import qualified Data.Map as Map
import Util

type Name = String

data Module = Module { functions :: [Function], functionMap :: Map Name Function, comments :: [Comment], streams :: [Stream], streamMap :: Map Name Stream, pipes :: [Pipe] } deriving (Eq, Ord, Show)

data Stream = Stream { streamName :: Name, streamSig :: TypeExpr }
  deriving (Eq, Ord, Show)
data Pipe = Pipe { funcName :: Name, inStreams :: [(Name, Integer)], outStreamName :: Name }
  deriving (Eq, Ord, Show)
newtype Comment = Comment String
  deriving (Eq, Ord, Show)

data Function = Function
  { name :: Name
  , signature :: TypeExpr
  , args :: [Name]
  , body :: Expr
  } deriving (Eq, Ord, Show)

data Stmt = F Function | S Stream | P Pipe | C Comment deriving (Eq, Show)

data TypeExpr
  = NumType
  | ByteType
  | AtomType
  | Unspecified Name
  | TupType TypeExpr TypeExpr
  | ListType TypeExpr
  | Arrow TypeExpr TypeExpr
  deriving (Eq, Ord, Show)

applyTypeExpr :: (TypeExpr -> TypeExpr) -> TypeExpr -> TypeExpr
applyTypeExpr f (TupType t1 t2) = TupType (f t1) (f t2)
applyTypeExpr f (ListType t) = ListType (f t)
applyTypeExpr f (Arrow tl tr) = Arrow (f tl) (f tr)
applyTypeExpr _ t = t

applyExpr :: (Expr -> Expr) -> Expr -> Expr
applyExpr f (Call n es) = Call n (fmap f es)
applyExpr f (UnOp u e) = UnOp u (f e)
applyExpr f (BinOp b e1 e2) = BinOp b (f e1) (f e2)
applyExpr f (TernOp t e1 e2 e3) = TernOp t (f e1) (f e2) (f e3)
applyExpr f e = e

data Value
  = Number Double -- 34.23
  | Byte Integer -- 'b'
  | Atom Name -- Apple
  | Tuple Expr Expr --- [1 'a']
  | List TypeExpr [Expr] --- num [1, 2, 3]
  deriving (Eq, Ord, Show)

data Expr
  = Val Value
  | Ident Name  -- arg
  | Call Name [Expr]  -- add 12 45 (function invocation)
  | UnOp UnOp Expr
  | BinOp Bop Expr Expr  -- + 2 3
  | TernOp Top Expr Expr Expr
  deriving (Eq, Ord, Show)

data UnOp
  = Fst
  | Snd
  | Head -- INTERNAL ONLY
  | Tail  -- INTERNAL ONLY
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

data Top
  = Uncons
  | If
  deriving (Eq, Ord, Show)
