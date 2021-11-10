module Test where

import Data.List
import Syntax
import Parser

main = do
  file <- readFile "src/examples/add.po"
  let res = runParser (oneOrMore parseStmt) mempty file
  print res
  return ()
