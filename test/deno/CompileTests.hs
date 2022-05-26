module CompileTests where

import Compile (runCompilation)

main :: IO ()
main = do
  runCompilation "test/deno/math.po" "test/deno/math.js"
  runCompilation "test/deno/hof.po" "test/deno/hof.js"
