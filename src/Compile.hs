module Compile where

import Data.Functor
import Data.Function
import Control.Applicative
import Syntax
import MiniParser
import Parser
import Typecheck (typecheckModule)
import Util (mapLeft)

newtype CompileError = CompileError String
  deriving (Show, Eq)

-- testing util
runp :: Parser a -> String -> Either ParseError (a, Cursor, String)
runp p = runParser p mempty

parse :: String -> Either CompileError Module
parse program =
  case runParser parseModule mempty program of
    Left err -> Left $ CompileError (show err)
    Right (stms, cursor, unparsed) -> if unparsed == "" then Right (Module stms) else Left . CompileError $ show (ParseError cursor)

typecheck :: Module -> Either CompileError Module
typecheck mod
  = typecheckModule mod
  & mapLeft (CompileError . show)

compile :: String -> Either CompileError String
compile program
  =   parse program
  >>= typecheck
  <&> show

save :: String -> Either CompileError String -> IO ()
save dest (Right js)
  =  writeFile dest js
  >> putStrLn ">\tSuccessfully Compiled!"
  >> putStrLn ("+\t" ++ dest)
save _ (Left err) = putStrLn ("!\t" ++ show err)

--sortpo = "src/examples/rsort.po"
--sortjs = "dest/rsport.js"

runCompilation :: String -> String -> IO ()
runCompilation src dest = do
  code <- readFile src <&> compile
  core <- readFile core
  save dest $ (++core) <$> code
    where
    core = "src/core.js"
