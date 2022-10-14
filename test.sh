#!/bin/bash

stack install && \
stack test && \
stack runhaskell test/deno/CompileTests && \
deno test && \
echo "ALL TESTS PASS"

