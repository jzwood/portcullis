module Transpile where

import Data.Functor
import Syntax
import MiniParser
import Parser


data CompileError = CompileError String
  deriving (Show, Eq)

-- testing util
runp :: Parser a -> String -> Either Cursor (a, Cursor, String)
runp p s = runParser p mempty s

parse :: String -> Either CompileError [Stmt]
parse program =
  case runParser parseModule mempty program of
    Left c -> Left $ CompileError (show $ ParseError c)
    Right (stms, _, _) -> Right stms

semanticCheck :: [Stmt] -> Either CompileError [Statement]
semanticCheck = undefined

typecheck :: [Statement] -> Either CompileError [Statement]
typecheck = undefined

transpile :: String -> Either CompileError String
transpile program
  =   parse program
  >>= semanticCheck
  >>= typecheck
  <&> unlines . (map show)
