#!/bin/bash

v="[A $(($RANDOM % 200)) $(($RANDOM % 200)), A $(($RANDOM % 200)) $(($RANDOM % 200))"

for i in $(seq 2 10 10003)
do
    a="$v]"

    #echo $a
    
    sum=0
    for j in {1..100}
    do
        t1=$(gdate +"%s%N")
        "./$1" "$a" > /dev/null
        t2=$(gdate +"%s%N")
        t=$(bc <<< "$t2 - $t1")
        sum=$(bc <<< "$sum + $t")
        echo -n "."
    done

########################

    for j in {1..10}
    do
       v="$v, A $(( $RANDOM % 200)) $(( $RANDOM % 200))"
    done

########################

    echo $(bc <<< "$i / 10.0")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$i,$t" >> time.csv

done
