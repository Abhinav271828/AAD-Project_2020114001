#!/bin/bash

for p in $(seq 1 10 100000)
do

    k=$((($RANDOM % p) + 1))
    echo "p = $p"
    echo "k = $k"

########################

    sum=0
    for j in {1..100}
    do
        t1=$(gdate +"%s%N")
        "./$1" "$k" "$p" > /dev/null
        t2=$(gdate +"%s%N")
        t=$(bc <<< "$t2 - $t1")
        sum=$(bc <<< "$sum + $t")
        echo -n "."
    done

########################

########################

    echo $(bc <<< "$p")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$p,$t" >> time.csv

done
