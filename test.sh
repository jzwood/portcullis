#!/bin/bash

if [ "$1" = "--all" ]; then
  stack test
else
  stack install
fi

stack runhaskell test/codegen/CompileTests && \
echo "JAVASCRIPT UNITTESTS" && \
deno test test/codegen/js && \
echo "PYTHON UNITTESTS" && \
python3 test/codegen/py/hof.test.py && \
python3 test/codegen/py/math.test.py && \
echo "LUA UNITTESTS" && \
lua test/codegen/lua/hof.test.lua && \
lua test/codegen/lua/math.test.lua && \
echo "SUCCESS!"
