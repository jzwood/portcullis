module Main where

import Compile (runCompilation)
import System.Environment ( getArgs, getProgName )

main :: IO ()
main = do
  argv <- getArgs
  progName <- getProgName
  case argv of
    [src, dest] -> runCompilation src dest
    _ -> putStrLn $ unlines [ "Usage:"
                            , "  " ++ unwords [ progName, "<src.po>", "<dest.js>" ]
                            , "  " ++ unwords [progName, "<src.po>", "<dest.py>" ]
                            ]
  return ()
