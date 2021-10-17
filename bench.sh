#!/bin/bash

arow="[$(($RANDOM % 10))"
brow="[$(($RANDOM % 10))"

arows=($arow)
brows=($brow)

for i in $(seq 1 1 151)
do
    echo "i = $i"
    amatr="["
    for r in "${arows[@]:0:${#arows[@]}-1}"
    do
        amatr="$amatr$r],"
    done
    r=${arows[${#arows[@]} - 1]}
    amatr="$amatr$r]]"
    bmatr="["
    for r in "${brows[@]:0:${#brows[@]}-1}"
    do
        bmatr="$bmatr$r],"
    done
    r=${brows[${#brows[@]} - 1]}
    bmatr="$bmatr$r]]"

    echo $amatr
    echo $bmatr

########################

    sum=0
    for j in {1..100}
    do
        t1=$(gdate +"%s%N")
        "./$1" "$amatr" "$bmatr" > /dev/null
        t2=$(gdate +"%s%N")
        t=$(bc <<< "$t2 - $t1")
        sum=$(bc <<< "$sum + $t")
        echo -n "."
    done

########################

    arows2=()
    for t in "${arows[@]}"
    do
        s="$t"
        for u in {1..1}
        do
            s="$s,$(($RANDOM % 10))"
        done
        arows2+=("$s")
    done
    arows=("${arows2[@]}")

    brows2=()
    for t in "${brows[@]}"
    do
        s="$t"
        for u in {1..1}
        do
            s="$s,$(($RANDOM % 10))"
        done
        brows2+=("$s")
    done
    brows=("${brows2[@]}")

    for k in {1..1}
    do
        arow="[$(($RANDOM % 10))"
        for j in $(seq 1 1 $(($i)))
        do
            arow="$arow,$(($RANDOM % 10))"
        done
        arows+=("$arow")
    done

    for k in {1..1}
    do
        brow="[$(($RANDOM % 10))"
        for j in $(seq 1 1 $(($i)))
        do
            brow="$brow,$(($RANDOM % 10))"
        done
        brows+=("$brow")
    done

##########################

    echo $(bc <<< "$i / 10.0")
    t=$(bc -l <<< "$sum / 100000.0")
    echo "$i,$t" >> time.csv
done
