#!/usr/bin/env bash

TESTS_VALID=tests/valid/*.txt
TESTS_ERR=tests/err/*.txt
APP=dist/build/syntax-analyzer/syntax-analyzer

cabal build

for filename in $TESTS_VALID; do
    if [ "$(./$APP $(realpath $filename))" == "$(cat tests/valid/ans/$(echo $(basename $filename) | cut -f 1 -d '.').ans)" ]; then
        echo "$filename OK"
    else 
        echo "$filename FAILED"
    fi
done

for filename in $TESTS_ERR; do
    ./$APP $(realpath $filename) > /dev/null 2> /dev/null 
    if ! [ $? -eq 0 ]; then
        echo "$filename OK"
    else 
        echo "$filename FAILED"
    fi
done

cabal clean
