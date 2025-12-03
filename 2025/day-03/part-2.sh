#!/bin/bash

joltage=0

solve_line() {
    line="$1"
    length=${#line}

    declare -A dp

    for ((i = 0; i <= length; i++)); do
        for ((j = 0; j <= 12; j++)); do
            dp[$i,$j]=0
        done
    done

    for ((i = 0; i < length; i++)); do
        for ((j = 0; j <= 12; j++)); do
            a=$((i+1))
            b=${dp[$a,$j]}
            dp[$a,$j]=$(( b > dp[$i,$j] ? b : dp[$i,$j] ))

            if (( j < 12)); then
                c=$((j+1))
                d=${dp[$a,$c]}
                e=$((10 * dp[$i,$j] + ${line:i:1}))
                dp[$a,$c]=$(( d > e ? d : e ))
            fi
        done
    done
    
    res=${dp[$length,12]}
    ((joltage += res))
}

while IFS="" read -r line || [ -n "$line" ]; do
    solve_line "$line"
done < input.txt

echo "Part 2: $joltage"