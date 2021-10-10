#!/bin/bash

for i in $(seq 0 1000 500000)
do
    sum=0
    for j in {1..100}
    do
        t1=$(python -c 'from time import time; print(time())')
        "./$1" "$i" > /dev/null
        t2=$(python -c 'from time import time; print(time())')
        t=$(bc <<< "$t2 - $t1")
        sum=$(bc <<< "$sum + $t")
        echo -n "."
    done
    echo $(bc <<< "$i / 1000")
    t=$(bc -l <<< "$sum / 100.0")
    echo "$i,$t" >> time.csv
done
