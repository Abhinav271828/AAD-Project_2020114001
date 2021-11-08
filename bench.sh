#!/bin/bash

v="[0,1,2,3"
#for i in $(seq 1 1 150)
#do
#    v="$v$i,"
#done
#v="$v""151"

for i in $(seq 4 1 200)
do
    verts="$v]"
    
    e=()
    
    for m in $(seq 0 1 $((i-1)))
    do
        for n in $(seq 0 1 $((i-1)))
        do
            r=$(($RANDOM % 100))
            if [[ $r -lt $2 ]]; then
              e+=( "(($m,$n),$(($RANDOM % 10)))" )
            fi
        done
    done

    edges="["
    for ed in "${e[@]:0:${#e[@]}-1}"
    do
        edges="$edges$ed,"
    done
    ed=${e[${#e[@]}-1]}
    edges="$edges$ed]"
    
    g="($verts,$edges)"

    echo "g = $g"

########################

    sum=0
    for j in {1..100}
    do
        t1=$(gdate +"%s%N")
        "./$1" "$g" > /dev/null
        t2=$(gdate +"%s%N")
        t=$(bc <<< "$t2 - $t1")
        sum=$(bc <<< "$sum + $t")
        echo -n "."
    done

########################

    #for k in $(seq $i 1 $((i)))
    #do
        v="$v,$i"
    #done

########################

    echo $(bc <<< "$i")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$i,$t" >> time.csv

done
