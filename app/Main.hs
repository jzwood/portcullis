module Main where

import Transpile (runTranspilation)
import System.Environment ( getArgs, getProgName )

main = do
  argv <- getArgs
  progName <- getProgName
  case argv of
    [src, dest] -> runTranspilation src dest
    _ -> (print $ unwords ["Usage:", progName, "<src.po>", "<dest.js>"])
  return ()
