#!/usr/bin/env bash

TESTS_VALID=tests/valid/*.txt
TESTS_ERR=tests/err/*.txt

cabal build

for filename in $TESTS_VALID; do
    cabal run $(realpath $filename) > /dev/null 2> /dev/null 
    if [ $? -eq 0 ]; then
        echo "$filename OK"
    else 
        echo "$filename FAILED"
    fi
done

for filename in $TESTS_ERR; do
    cabal run $(realpath $filename) > /dev/null 2> /dev/null 
    if ! [ $? -eq 0 ]; then
        echo "$filename OK"
    else 
        echo "$filename FAILED"
    fi
done

cabal clean
