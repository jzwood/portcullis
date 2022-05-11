#!/bin/bash

find app src -type f -d 1 -name \*.hs | xargs cat | grep -E "[^\n]" | grep -Ev "^\s*--" | wc -l
