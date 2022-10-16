#!/bin/bash

stack install && \
stack test && \
stack runhaskell test/codegen/CompileTests && \
deno test test/codegen/js && \
python3 test/codegen/py/hof.test.py && \
echo "SUCCESS!"
