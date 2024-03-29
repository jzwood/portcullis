module CompileTests where

import Compile (runCompilation)
import Data.List (intercalate)
import Data.Char (toUpper)
import System.Directory (createDirectory, listDirectory, removePathForcibly)
import System.FilePath ((</>), (<.>), replaceExtension, isExtensionOf)

compile :: FilePath -> FilePath -> String -> FilePath -> IO ()
compile src dest ext file = runCompilation (src </> file) (dest </> replaceExtension file ('.' : ext))

compileDir :: FilePath -> FilePath -> String -> IO ()
compileDir dirPath destPath ext
  = listDirectory dirPath
  >>= mapM_ (compile dirPath destPath ext) . filter ("po" `isExtensionOf`)

pathify :: [String] -> String
pathify = intercalate "/"

testCodeGen :: String -> IO ()
testCodeGen ext = do
  putStrLn "\tCLEANING PREVIOUS RUN…"
  removePathForcibly $ pathify ["test", "codegen", ext, "compiled"]
  createDirectory $ pathify ["test", "codegen", ext, "compiled"]
  putStrLn $ unwords ["\tCOMPILING", toUpper <$> ext ]
  runCompilation "test/codegen/math.po" $ pathify ["test", "codegen", ext, "compiled", "math." ++ ext]
  runCompilation "test/codegen/hof.po" $ pathify ["test", "codegen", ext, "compiled", "hof." ++ ext]
  -- all example portcullis programs should compile
  compileDir "test/codegen/coverage" (pathify ["test", "codegen", ext, "compiled"]) ext
  compileDir "test/codegen/examples" (pathify ["test", "codegen", ext, "compiled"]) ext

main :: IO ()
main = do
  testCodeGen "js"
  testCodeGen "py"
  testCodeGen "lua"
