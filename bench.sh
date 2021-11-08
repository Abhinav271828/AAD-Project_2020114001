#!/bin/bash

function tree
{
    if [[ $1 -eq 0 ]]; then
        t="Leaf $2"
        n="$2"
        echo "$n $t"
    else
       t1=$(tree $(($1-1)) $(($2+1)))

       c=0
       i=""
       s1=""
       for d in $t1
       do
         if [[ $c -eq 0 ]]; then
           if [[ $d =~ [0-9]+ ]]; then
             i+=$d
           else
             c=1
             s1+="$d "
           fi
         else
           s1+="$d "
         fi
       done

       t2=$(tree $(($1-1)) $(($i+1)))

       c=0
       j=""
       s2=""
       for d in $t2
       do
         if [[ $c -eq 0 ]]; then
           if [[ $d =~ [0-9]+ ]]; then
             j+=$d
           else
             c=1
             s2+="$d "
           fi
         else
           s2+="$d "
         fi
       done

       t="Node $2 [$s1, $s2]"
       echo "$j $t"
    fi
}

for i in $(seq 0 1 1000)
do
    t=$(tree $i 0)
    
    c=0
    s=""
    for d in $t
    do
        if [[ $c -eq 0 ]]; then
            if [[ ! ( $d =~ [0-9]+ ) ]]; then
                c=1
                s+="$d "
            fi
        else
            s+="$d "
        fi
    done

    echo "s = $s"

########################

    sum=0
    for j in {1..100}
    do
        t1=$(gdate +"%s%N")
        "./$1" "$s" > /dev/null
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

done

