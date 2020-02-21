#!/bin/bash

WORKING_DIR="./team/hc-logs"

filterFiles() {
    # Yes, the line exceeds the max length. But to flex the awesomeness of this command, it is kept on a single line :cool:
    find . -wholename "$WORKING_DIR/unpack*" -type f | xargs sed -i -Ee 's/^iteration:([0-9]*)\s.*best_value:([0-9]*).*|.*/\1 \2/g' -e '/^ $/d'
}

generateFinalFiles() {
    # loop over all unpackfiles and loop over all its runs -> output this to a single final file per unpackfile.
    for var in {1..6}; do
        find . -wholename "$WORKING_DIR/unpackfile$var*" -type f | xargs awk '{ print $2 }' >>"$WORKING_DIR/final$var.dat"
    done
}

calculateMetrics() {

    # Calculate the standard deviation and average value for each final file.
    # Results are saved to result.dat
    for var in {1..6}; do
        cat "$WORKING_DIR/final$var.dat" | datamash mean 1 sstdev 1 >>"$WORKING_DIR/result.dat"
        sed -i -Ee "$var s/^([0-9]*,[0-9]*)\s*([0-9]*,[0-9]*)/$var param:$var \1 \2/g" "$WORKING_DIR/result.dat"
    done
}

filterFiles
generateFinalFiles
calculateMetrics
