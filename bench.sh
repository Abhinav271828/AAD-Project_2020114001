#!/bin/bash

d=$((RANDOM % 10))
a="[($(($RANDOM % 10)),$d)"

for i in $(seq 1 3 1000)
do
    m="$a]"
    
    echo "m = $m"

########################

    sum=0
    for j in {1..100}
    do
        t1=$(gdate +"%s%N")
        "./$1" "$m" > /dev/null
        t2=$(gdate +"%s%N")
        t=$(bc <<< "$t2 - $t1")
        sum=$(bc <<< "$sum + $t")
        echo -n "."
    done

########################

    for k in $(seq $i 1 $((i+3-1)))
    do
        e=$(($RANDOM % 10))
        a="$a,($d,$e)"
        d=$e
    done

########################

    echo $(bc <<< "$i")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$i,$t" >> time.csv

done
