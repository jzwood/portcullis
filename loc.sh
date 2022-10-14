#!/bin/bash

LOC=$(find app src -type f -name \*.hs | xargs cat | grep -E "[^\n]" | grep -Ev "^\s*--" | wc -l | xargs)

echo "$LOC loc"
