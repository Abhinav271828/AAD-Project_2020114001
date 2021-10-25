#!/bin/bash

v="[$(($RANDOM % 10))"

for i in $(seq 2 5 1000)
do
    l="$v]"
    
    echo "l = $l"

########################

    sum=0
    for j in {1..100}
    do
        t1=$(gdate +"%s%N")
        "./$1" "$l" > /dev/null
        t2=$(gdate +"%s%N")
        t=$(bc <<< "$t2 - $t1")
        sum=$(bc <<< "$sum + $t")
        echo -n "."
    done

########################

    for k in $(seq $i 1 $((i+3-1)))
    do
        v="$v,$(($RANDOM % 10))"
    done

########################

    echo $(bc <<< "$i")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$i,$t" >> time.csv

done
