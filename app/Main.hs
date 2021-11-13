module Main where

import Syntax
import Parser
import MiniParser

main = do
  file <- readFile "src/examples/add.po"
  let res = runParser (oneOrMore parseStmt) mempty file
  print res
  return ()
