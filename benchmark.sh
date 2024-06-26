#!/bin/bash

for cpp_file in generated/job/*.cpp; do
    echo $cpp_file
    g++ $cpp_file -O3 -std=c++17
    ./a.out
    echo "--------------------"
done | tee results.txt
