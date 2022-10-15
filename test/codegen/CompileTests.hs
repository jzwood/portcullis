module CompileTests where

import Compile (runCompilation)
import System.Directory (createDirectory, listDirectory, removePathForcibly)
import System.FilePath ((</>), (<.>), replaceExtension, isExtensionOf)

compile :: FilePath -> FilePath -> FilePath -> IO ()
compile src dest file = runCompilation (src </> file) (dest </> replaceExtension file ".js")

compileDir :: FilePath -> IO ()
compileDir dirPath
  = listDirectory dirPath
  >>= mapM_ (compile dirPath "test/codegen/compiled") . filter ("po" `isExtensionOf`)

main :: IO ()
main = do
  removePathForcibly "test/codegen/compiled"
  createDirectory "test/codegen/compiled"
  runCompilation "test/codegen/math.po" "test/codegen/compiled/math.js"
  runCompilation "test/codegen/hof.po" "test/codegen/compiled/hof.js"
  -- all example portcullis programs should compile
  compileDir "test/codegen/coverage"
  compileDir "test/codegen/examples"
