module CompileTests where

import Compile (runCompilation)

main :: IO ()
main = do
  runCompilation "test/deno/math.po" "test/deno/compiled/math.js"
  runCompilation "test/deno/hof.po" "test/deno/compiled/hof.js"
