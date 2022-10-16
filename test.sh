#!/bin/bash

stack install && \
stack test && \
stack runhaskell test/codegen/CompileTests && \
deno test test/codegen/js && \
echo "SUCCESS!"
