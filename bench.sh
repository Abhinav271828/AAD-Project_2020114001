#!/bin/bash

a="[$(($RANDOM % 10))"
b="[$(($RANDOM % 10))"

for i in $(seq 1 5 10000)
do
    m="$a]"
    n="$b]"
    
    echo "m = $m"
    echo "n = $n"

########################

    sum=0
    for j in {1..100}
    do
        t1=$(gdate +"%s%N")
        "./$1" "$m" "$n" > /dev/null
        t2=$(gdate +"%s%N")
        t=$(bc <<< "$t2 - $t1")
        sum=$(bc <<< "$sum + $t")
        echo -n "."
    done

########################

    for k in $(seq $i 1 $((i+5-1)))
    do
        a="$a,$(($RANDOM % 10))"
        b="$b,$(($RANDOM % 10))"
    done

########################

    echo $(bc <<< "$i")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$i,$t" >> time.csv

done
