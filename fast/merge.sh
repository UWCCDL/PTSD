#!/bin/bash

if [ -f sims2.partial ]; then
    rm sims2.partial
fi

if [ -f sims2.csv ]; then
    rm sims2.csv
fi

for f in simulations2/*.csv; do
    N=`wc $f | awk '{print $1}'`;
    M=$((N-1)); #echo $M, $N;
    tail -${M} $f >> sims2.partial;
done

head -1 simulations2/bll-0.5_rt-0.0_w-2.5_attributes-3_ef-20_n-50_ptes-0.50_ptev-20_rf-0_slots-8.csv > header.txt

cat header.txt sims2.partial > sims2.csv
