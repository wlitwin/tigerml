#!/bin/sh

for file in `ls ../testcases/*.tig | sort -V`; do
    echo Testing $file
    ./main.native < $file
    echo
done;
