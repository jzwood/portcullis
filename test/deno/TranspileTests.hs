module TranspileTests where

import Transpile

main :: IO ()
main = do
  runTranspilation "test/deno/math.po" "test/deno/math.js"
  runTranspilation "test/deno/hof.po" "test/deno/hof.js"
