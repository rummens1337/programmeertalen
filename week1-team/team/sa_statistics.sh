#!/bin/bash

WORKING_DIR="./team/sa-logs"

# Finds all unpack files and filters all unnecessary information.
# Result: each line is now "{iteration} {knapsack_value}"
# Example line 1: "1 416651"
filterFiles() {
    find . -wholename "$WORKING_DIR/unpack*" -type f |
        xargs sed -i -Ee 's/^iteration:([0-9]*)\s.*best_value:([0-9]*).*|.*/\1 \2/g' -e '/^ $/d'
}

# Generates one file per unpackfile{N}, summing all values of all runs per unpackfile.
# This results in 5 "final{N}" files, each containing the total sum of their respective unpackfile.
# Result: each line now contains one value "{knapsack_value}" for a total of 50.000 lines.
generateFinalFiles() {
    for var in {1..6}; do
        find . -wholename "$WORKING_DIR/unpackfile$var*" -type f |
            xargs awk '{ print $2 }' >>"$WORKING_DIR/final$var.dat"
    done
}

# Calculate the standard deviation and average value for each final{N} file.
# Results are saved to result.dat
calculateMetrics() {
    for var in {1..6}; do
        cat "$WORKING_DIR/final$var.dat" | datamash mean 1 sstdev 1 >>"$WORKING_DIR/result.dat"
        sed -i -Ee "$var s/^([0-9]*,[0-9]*)\s*([0-9]*,[0-9]*)/$var param:$var \1 \2/g" "$WORKING_DIR/result.dat"
    done
}

# Order of execution.
filterFiles
generateFinalFiles
calculateMetrics
