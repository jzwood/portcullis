module CompileTests where

import Compile (runCompilation)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>), (<.>), replaceExtension, isExtensionOf)

compile :: FilePath -> FilePath -> FilePath -> IO ()
compile src dest file = runCompilation (src </> file) (dest </> replaceExtension file ".js")

main :: IO ()
main = do
  createDirectoryIfMissing False "test/deno/compiled"
  runCompilation "test/deno/math.po" "test/deno/compiled/math.js"
  runCompilation "test/deno/hof.po" "test/deno/compiled/hof.js"
  -- all example portcullis programs should compile
  examples <- filter ("po" `isExtensionOf`) <$> listDirectory "examples"
  mapM_ (compile "examples" "test/deno/compiled") examples
