#!/bin/bash
# ------------------------------------------------------------------ #
# Usage
#
#   $ merge.sh <folder> > <outputfile>
#
# ------------------------------------------------------------------ #

DIR=$1

# Cleanup first (just in case previous run ended badly) 
if [ -f sims.partial ]; then
    rm sims.partial
fi

if [ -f header.partial ]; then
    rm header.partial
fi

# Collect all data from all simulations
for f in $DIR/*.csv; do
    N=`wc $f | awk '{print $1}'`;
    M=$((N-1)); #echo $M, $N;
    tail -${M} $f >> sims.partial;

    if [ ! -f header.partial ]; then
        head -1 $f > header.partial
    fi
done

# Print on screen for redirect
cat header.partial sims.partial

# Cleanup afterwards
if [ -f sims.partial ]; then
    rm sims.partial
fi

if [ -f header.partial ]; then
    rm header.partial
fi
