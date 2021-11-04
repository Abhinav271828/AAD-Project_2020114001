#!/bin/bash

a="[($((($RANDOM % 10) + 1)),$((($RANDOM % 10) + 1)))"

for i in $(seq 1 3 302)
do
    m="$a]"
    n=$(($i * 10))
    
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

    for k in $(seq $i 1 $((i+3-1)))
    do
        e=$((($RANDOM % 10) + 1))
        f=$((($RANDOM % 10) + 1))
        a="$a,($e,$f)"
    done

########################

    echo $(bc <<< "$i")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$i,$t" >> time.csv

done
