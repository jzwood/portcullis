#!/bin/bash

find src -type f | xargs cat | grep -E "[^\n]" | wc -l
