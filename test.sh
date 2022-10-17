#!/bin/bash

stack install && \
stack test && \
stack runhaskell test/codegen/CompileTests && \
echo "JAVASCRIPT UNITTESTS" && \
deno test test/codegen/js && \
echo "PYTHON UNITTESTS" && \
python3 test/codegen/py/hof.test.py && \
python3 test/codegen/py/math.test.py && \
echo "SUCCESS!"
