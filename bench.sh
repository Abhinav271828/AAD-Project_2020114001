#!/bin/bash

i=0
while read l
do
    #t=$(tree $i 0)
    #
    #c=0
    #s=""
    #for d in $t
    #do
    #    if [[ $c -eq 0 ]]; then
    #        if [[ ! ( $d =~ [0-9]+ ) ]]; then
    #            c=1
    #            s+="$d "
    #        fi
    #    else
    #        s+="$d "
    #    fi
    #done

    #echo "s = $s"

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

########################

    echo $(bc <<< "$i")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$i,$t" >> time.csv
    i=$((i+1))

done < trials.txt
