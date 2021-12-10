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


main = do
  print x
  print y
  print z
  print checked
  --file <- readFile "src/examples/add.po"
  --let res = runParser (oneOrMore parseStmt) mempty file
  --print res
  --return ()
