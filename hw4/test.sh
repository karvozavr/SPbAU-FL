#!/usr/bin/env bash

TESTS_VALID=tests/valid/*.txt
TESTS_ERR=tests/err/*.txt

source venv/bin/activate

pytest

if [ $? -eq 0 ]; then
    echo -e "\n\nUnit tests passed.\n\n"
else
    echo -e "\n\nUnit tests failed.\n\n"
fi

for filename in $TESTS_VALID; do
    output=$(python3 main.py --file $(realpath $filename) > /dev/null 2> /dev/null)
    correct=$(cat "tests/valid/ans/$(basename $filename).ans")
    if [ $? -eq 0 ]; then
        echo "$filename OK"
    else
        echo "$filename FAILED"
    fi
done

for filename in $TESTS_ERR; do
    python3 main.py --file $(realpath $filename) > /dev/null 2> /dev/null
    if [ $? -ne 0 ]; then
        echo "$filename OK"
    else
        echo "$filename FAILED"
    fi
done