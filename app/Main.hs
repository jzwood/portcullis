module Main where

import Compile (runCompilation)
import System.Environment ( getArgs, getProgName )

main = do
  argv <- getArgs
  progName <- getProgName
  case argv of
    [src, dest] -> runCompilation src dest
    _ -> (print $ unwords ["Usage:", progName, "<src.po>", "<dest.js>"])
  return ()
