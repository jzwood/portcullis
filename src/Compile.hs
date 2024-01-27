{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Compile where

import Data.Functor
import Data.Function
import Data.Char (toLower)
import Control.Applicative
import System.FilePath (takeExtension, takeBaseName)
import qualified Data.ByteString.UTF8 as BSU
import Data.FileEmbed (embedFile)
import Syntax
import qualified MiniParser as ParseLib
import MiniParser (runParser, Parser, Cursor)
import Parser
import CodeGen.Js.Target
import CodeGen.Erl.Target
import CodeGen.Py.Target
import CodeGen.Lua.Target
import CodeGen.Po.Target
import CodeGen.Html.Target
import CodeGen.Mermaid.Target
import qualified Typecheck
import Typecheck (typecheckModule)
import Utils (mapLeft, unlines', paren)
import Paths_portcullis

data CompileError = ParseError ParseLib.ParseError | TypecheckError [Typecheck.TypecheckError]
  deriving (Eq, Show)

-- testing util
runp :: Parser a -> String -> Either ParseLib.ParseError (a, Cursor, String)
runp p = runParser p mempty


readCore :: String -> IO String
readCore path = getDataFileName path >>= readFile

parse :: String -> Either CompileError Module
parse program =
  case runParser parseProgram mempty program of
    Left err -> Left $ Compile.ParseError err
    Right (mod, cursor, unparsed) -> if unparsed == "" then Right mod else Left . Compile.ParseError $ ParseLib.ParseError cursor

typecheck :: Module -> Either CompileError Module
typecheck mod
  = typecheckModule mod
  & mapLeft TypecheckError

compile :: String -> Either CompileError Module
compile program
  =   parse program
  >>= typecheck

save :: String -> String -> Either CompileError String -> IO ()
save _ dest (Right js)
  =  writeFile dest js
  >> putStrLn ("✓\t" ++ dest ++ " Successfully Compiled")
save src _ (Left err) = putStrLn ("!\t" ++ src ++ " " ++ show err)

toJsFileX :: Module -> IO String
toJsFileX mod = readCore "src/CodeGen/Js/core.js"
  <&> (\x -> unlines' [x, toJs mod])

toJsFile :: Module -> String
toJsFile mod = unlines' [BSU.toString $(embedFile "src/CodeGen/Js/core.js"), toJs mod]

toPyFile :: Module -> String
toPyFile mod = unlines' [BSU.toString $(embedFile "src/CodeGen/Py/core.py"), toPy mod]

toLuaFile :: Module -> String
toLuaFile mod = unlines' [BSU.toString $(embedFile "src/CodeGen/Lua/core.lua"), toLua mod]

toErlFile :: String -> Module -> String
toErlFile src mod = unlines' [concat ["-module", paren src, "."], "", BSU.toString $(embedFile "src/CodeGen/Erl/core.erl"), toErl mod]

toPoFile :: Module -> String
toPoFile = toPo

toHtmlFile :: Module -> String
toHtmlFile = toHtml

toMermaidFile :: Module -> String
toMermaidFile = toMermaid

runCompilation :: String -> String -> IO ()
runCompilation src dest = do
  code <- readFile src <&> compile
  let writeFile = save src dest
  case takeExtension dest of
    ".js" -> writeFile $ toJsFile <$> code
    ".lua" -> writeFile $ toLuaFile <$> code
    ".py" -> writeFile $ toPyFile <$> code
    ".erl" -> writeFile $ toErlFile (takeBaseName src <&> toLower) <$> code
    ".po" -> writeFile $ toPoFile <$> code
    ".html" -> writeFile $ toHtmlFile <$> code
    ".mmd" -> writeFile $ toMermaidFile <$> code
    ext -> putStrLn $ unwords [ "Unrecognized extension:", ext ]
