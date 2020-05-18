#!/bin/bash

threads=(1 8 16 40)
size=(1 5 25 100)
class=("Synchronized" "Unsynchronized" "AcmeSafe")

printf "Class Array.Size Threads Real.Time(s) CPU.Time(s) Avg.Swap.Time.Real(ns) Avg.Swap.Time.CPU(ns) Mismatch\n" > data.txt
for c in ${class[@]}
do
    for s in ${size[@]}
    do
	for t in ${threads[@]}
	do
	    printf "$c $s $t " >> data.txt
	    str=`time timeout 3600 java UnsafeMemory $c $t 100000000 $s 2>&1`
	    echo $str | grep -o "[0-9]*\.[0-9]*" | sed -n '1,4p' | tr '\n' ' ' >> data.txt
	    if echo $str | grep -e "mismatch"; then
		echo " 1" >> data.txt
	    else
		echo " 0" >> data.txt
	    fi
	done
    done
done

echo "TEST FINISHED RESULTS IN data.txt"

