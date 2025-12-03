#!/bin/bash

joltage=0

while IFS="" read -r line || [ -n "$line" ]
do
    length=${#line}
    max1=${line:0:1}
    max1idx=0
    for ((i = 1; i < length; i++)); do
        char="${line:i:1}"
        if (( char > max1 )); then
            max1=$char
            max1idx=$i
        fi
    done
    if (( max1idx + 1 >= length )); then
        max1=${line:0:1}
        max1idx=0
        for ((i = 1; i < length - 1; i++)); do
            char="${line:i:1}"
            if (( char > max1 )); then
                max1=$char
                max1idx=$i
            fi
        done
    fi
    max2=${line:((max1idx+1)):1}
    for ((i = max1idx+2; i < length; i++)); do
        char="${line:i:1}"
        if (( char > max2 )); then
            max2=$char
        fi
    done
    (((joltage += max1 * 10 + max2)))
done < input.txt

echo "Part 1: $joltage"