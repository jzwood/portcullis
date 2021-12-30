module Transpile where

import Data.Functor
import Data.Function
import Control.Applicative
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
    Right (stms, cursor, unparsed) -> if unparsed == "" then Right stms else Left . CompileError $ show (ParseError cursor)

semanticCheck :: [Stmt] -> Either CompileError [Statement]
semanticCheck stmts = undefined

typecheck :: [Statement] -> Either CompileError [Statement]
typecheck = undefined

transpile :: String -> Either CompileError String
transpile program
  =   parse program
  -- >>= semanticCheck
  -- >>= typecheck
  <&> unlines . (map show)

handle :: String -> Either CompileError String -> IO ()
handle dest (Right js)
  =  writeFile dest js
  >> putStrLn ">\tSuccessfully Transpiled!"
  >> putStrLn ("+\t" ++ dest)
handle _ (Left err) = putStrLn ("!\t" ++ show err)

sortpo = "src/examples/rsort.po"
sortjs = "dest/rsport.js"

runTranspilation :: String -> String -> IO ()
runTranspilation src dest = do
  code <- readFile src <&> transpile
  core <- readFile core
  handle dest $ (core++) <$> code
    where
    core = "src/core.js"
