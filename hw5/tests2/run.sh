#!/bin/bash

for t in *.txt;
do
  echo "input:"
  cat $t
  echo $'\noutput:'
  ../main.py --file $t
  echo $'\n________________________________________________________________________________'
done
