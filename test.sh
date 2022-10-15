#!/bin/bash

stack install && \
stack test && \
stack runhaskell test/codegen/CompileTests && \
deno test && \
echo "ALL TESTS PASS"

