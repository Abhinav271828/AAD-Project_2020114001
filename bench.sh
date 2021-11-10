#!/bin/bash

a=$(($RANDOM % 10))
b=$(($RANDOM % 10))

for i in $(seq 1 100 100000)
do

    echo "a = $a"
    echo "b = $b"

########################

    sum=0
    for j in {1..100}
    do
        t1=$(gdate +"%s%N")
        "./$1" "$a" "$b" > /dev/null
        t2=$(gdate +"%s%N")
        t=$(bc <<< "$t2 - $t1")
        sum=$(bc <<< "$sum + $t")
        echo -n "."
    done

########################

for j in $(seq 0 1 99)
do
    a+=$(($RANDOM % 10))
    b+=$(($RANDOM % 10))
done

########################

    echo $(bc <<< "$i")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$i,$t" >> time.csv

done
