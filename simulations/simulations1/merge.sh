#!/bin/bash

# ------------------------------------------------------------------ #
# MERGE
# ------------------------------------------------------------------ #
# Merges different simulation files into a single file. Writes the
# output to STDOUT.
# ------------------------------------------------------------------ #
# Usage:
#
# $ ./merge.sh > {myoutputfile}
#
# ------------------------------------------------------------------ #

# This variable keeps track of whether the "first" file in a set has
# been processed or not. The header of the first file will be included
# in the output (for column names).

first=1

# Loops over partial simulations and cat the last N-1 lines from all of
# them (removing the header).

for file in simulations*.txt; do
    if [ "$first" -eq "1" ]; then
        head -1 $file
        first=0
    fi
    L=`wc $file | awk '{print $1}'`
    N=$((L - 1))
    # Uncomment the following line for verbose debug 
    # echo $L , $N
    
    # Comment the following line if you want to check
    # that the script is working, instead of running it.
    tail -$N $file
done
