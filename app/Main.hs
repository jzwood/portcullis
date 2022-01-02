module Main where

import Syntax
import Parser
import MiniParser
import Data.Map
import Typecheck

t = Arrow AtomType CharType
s = Arrow (Unspecfied "a") (Unspecfied "b")
checked = typecheck t s Data.Map.empty
x = typecheck AtomType (Unspecfied "a") Data.Map.empty
y = typecheck CharType (Unspecfied "b") (Data.Map.fromList [("a", AtomType)])
z = x >>= typecheck CharType (Unspecfied "b")

--t = TernOp Fold (Val $ Number 1) (Val $ Number 1) (Val $ Number 1)

func = unlines [ "alg -> Num -> Num Num"
               , "alg a b = + b a"

               , "sum -> [Num] Num"
               , "sum xs = fold alg 0 xs"
               ]
mod1 = runParser parseModule mempty func

--statement = Function
  --{ name :: Name
  --, signature :: TypeExpr
  --, args :: [Var]
  --, body :: Expr
  --} deriving (Show, Eq)

main = do
  print x
  print y
  print z
  print mod1
  --print checked
  --file <- readFile "src/examples/add.po"
  --let res = runParser (oneOrMore parseStmt) mempty file
  --print res
  --return ()
