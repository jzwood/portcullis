{-# LANGUAGE OverloadedStrings #-}

module Compile where

import CodeGen.Ex.Target
import CodeGen.Html.Target
import CodeGen.Js.Target
import CodeGen.Lua.Target
import CodeGen.Mermaid.Target
import CodeGen.Po.Target
import CodeGen.Py.Target
import Control.Applicative
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (toUpper)
import Data.Function
import Data.Functor
import MiniParser (Cursor, Parser, runParser)
import qualified MiniParser as ParseLib
import Parser
import Paths_portcullis
import Syntax
import System.FilePath (takeBaseName, takeExtension)
import Typecheck (typecheckModule)
import qualified Typecheck
import Utils (mapLeft, paren, unlines')

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
typecheck mod =
    typecheckModule mod
        & mapLeft TypecheckError

compile :: String -> Either CompileError Module
compile program =
    parse program
        >>= typecheck

save :: String -> String -> Either CompileError String -> IO ()
save _ dest (Right js) =
    writeFile dest js
        >> putStrLn ("✓\t" ++ dest ++ " Successfully Compiled")
save src _ (Left err) = putStrLn ("!\t" ++ src ++ " " ++ show err)

toJsFile :: String -> Module -> String
toJsFile core mod = unlines' [core, toJs mod]

toPyFile :: String -> Module -> String
toPyFile core mod = unlines' [core, toPy mod]

toLuaFile :: String -> Module -> String
toLuaFile core mod = unlines' [core, toLua mod]

toExFile :: String -> String -> Module -> String
toExFile dest core mod = unlines' [unwords ["defmodule", pathToModuleName $ takeBaseName dest, "do"], "", core, toEx mod, "end"]

toHtmlFile :: String -> Module -> String
toHtmlFile head mod = "<!DOCTYPE html>" ++ tag "html" [] (head ++ toHtml mod)

runCompilation :: String -> String -> IO ()
runCompilation src dest = do
    code <- readFile src <&> compile
    jsCore <- readCore "src/CodeGen/Js/core.js"
    exCore <- readCore "src/CodeGen/Ex/core.ex"
    luaCore <- readCore "src/CodeGen/Lua/core.lua"
    pyCore <- readCore "src/CodeGen/Py/core.py"
    htmlHead <- readCore "src/CodeGen/Html/head.html"
    let writeFile = save src dest
    case takeExtension dest of
        ".js" -> writeFile $ toJsFile jsCore <$> code
        ".lua" -> writeFile $ toLuaFile luaCore <$> code
        ".py" -> writeFile $ toPyFile pyCore <$> code
        ".ex" -> writeFile $ toExFile dest exCore <$> code
        ".po" -> writeFile $ toPo <$> code
        ".html" -> writeFile $ toHtmlFile htmlHead <$> code
        ".mmd" -> writeFile $ toMermaid <$> code
        ext -> putStrLn $ unwords ["Unrecognized extension:", ext]
