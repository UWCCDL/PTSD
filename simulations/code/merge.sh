#!/bin/bash
# ------------------------------------------------------------------ #
# Usage
#
#   $ merge.sh <folder> > <outputfile>
#
# ------------------------------------------------------------------ #

DIR=$1   # First argument is the folder to read the data from

# Cleanup first (just in case previous run ended badly) 
#
if [ -f sims.partial ]; then
    rm sims.partial
fi

if [ -f header.partial ]; then
    rm header.partial
fi

# Collect all data from all simulations into temporary header and
# data files.
#
for f in $DIR/*.csv; do
    N=`wc $f | awk '{print $1}'`;  # Total number of lines
    M=$((N-1));                    # Number of lines minus the header
    
    tail -${M} $f >> sims.partial; # Collect data into temp data file

    if [ ! -f header.partial ]; then
        head -1 $f > header.partial
    fi
done

# Print on screen for redirect
#
cat header.partial sims.partial

# Cleanup the temporary files afterwards
# 
if [ -f sims.partial ]; then
    rm sims.partial
fi

if [ -f header.partial ]; then
    rm header.partial
fi
