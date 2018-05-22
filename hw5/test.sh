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

