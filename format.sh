#!/bin/bash

base=$1
if [ "$base" = "" ]; then
  base="."
fi

for po in $(find $base -name "*.po" -type f); do
    portcullis-exe "$po" "$po" &
done
