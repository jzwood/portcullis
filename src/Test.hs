module Test where

import Data.List
import Syntax
import Parser

main = do
  file <- readFile "src/examples/guard.po"
  let res = runParser (oneOrMore parseFunc) mempty file
  print res
  return ()
