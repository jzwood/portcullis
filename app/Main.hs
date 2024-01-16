module Main where

import Compile (runCompilation)
import System.Environment (getArgs, getProgName)

exts = [".js", ".py", ".lua", ".po", ".html", ".mmd"]

main :: IO ()
main = do
    argv <- getArgs
    progName <- getProgName
    case argv of
        [src, dest] -> runCompilation src dest
        _usage -> putStrLn $ unlines ("Usage:" : fmap (\ext -> unwords [progName, "<src.po>", concat ["<dest", ext, ">"]]) exts)
    return ()
