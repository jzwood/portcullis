module Compile where

import Data.Functor
import Data.Function
import Control.Applicative
import Syntax
import qualified MiniParser as ParseLib
import MiniParser (runParser, Parser, Cursor)
import Parser
import qualified Typecheck
import Typecheck (typecheckModule)
import Util (mapLeft)

data CompileError = ParseError ParseLib.ParseError | TypecheckError [Typecheck.TypecheckError]
  deriving (Eq, Show)

-- testing util
runp :: Parser a -> String -> Either ParseLib.ParseError (a, Cursor, String)
runp p = runParser p mempty

parse :: String -> Either CompileError Module
parse program =
  case runParser parseProgram mempty program of
    Left err -> Left $ Compile.ParseError err
    Right (mod, cursor, unparsed) -> if unparsed == "" then Right mod else Left . Compile.ParseError $ ParseLib.ParseError cursor

typecheck :: Module -> Either CompileError Module
typecheck mod
  = typecheckModule mod
  & mapLeft TypecheckError

compile :: String -> Either CompileError String
compile program
  =   parse program
  >>= typecheck
  <&> show

save :: String -> String -> Either CompileError String -> IO ()
save _ dest (Right js)
  =  writeFile dest js
  >> putStrLn ("✓\t" ++ dest ++ " Successfully Compiled")
save src _ (Left err) = putStrLn ("!\t" ++ src ++ " " ++ show err)

runCompilation :: String -> String -> IO ()
runCompilation src dest = do
  code <- readFile src <&> compile
  core <- ('\n':) <$> readFile core
  save src dest $ (++core) <$> code where
    core = "src/core.js"
