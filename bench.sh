#!/bin/bash

a="100"
v="[(\"$a\", $(($RANDOM % 10)))"

for i in $(seq 1 1 1)
do
    a=$((a+1))
    v=$v",(\"$a\", $(($RANDOM % 10)))"
done

for i in $(seq 2 10 2000)
do
    x="$v]"

    #echo $x

    sum=0
    for j in {1..100}
    do
        t1=$(gdate +"%s%N")
        "./$1" "$x" > /dev/null
    #    echo $(bc -l <<< "l($i)/l(2)")
        t2=$(gdate +"%s%N")
        t=$(bc <<< "$t2 - $t1")
       sum=$(bc <<< "$sum + $t")
        echo -n "."
    done

########################

    for j in {1..10}
    do
       a=$(($a+1))
       v="$v, (\"$a\", $(( $RANDOM % 10)))"
    done

########################

    echo $(bc <<< "$i")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$i,$t" >> time.csv

done
