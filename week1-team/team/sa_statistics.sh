#!/bin/bash

WORKING_DIR="./sa-logs"

filterFiles(){
    # Yes, the line exceeds the max length. But to flex the awesomeness of this command, it is kept on a single line :cool:
    find . -wholename "$WORKING_DIR/unpack*" -type f | xargs sed -i -Ee 's/^iteration:([0-9]*)\s.*best_value:([0-9]*).*|.*/\1 \2/g' -e '/^ $/d'
}


generateParameterFiles(){
    for var in 1 2 3 4 5 6
    do
        find . -wholename "$WORKING_DIR/unpackfile$var*" -type f | xargs awk '{ print $2 }' >> "$WORKING_DIR/final$var.dat"
    done
}

calculateMetrics(){
    for var in 1 2 3 4 5 6
    do
        cat "$WORKING_DIR/final$var.dat" | datamash  mean 1 sstdev 1 >> "$WORKING_DIR/result.dat"
    done
}


filterFiles
generateParameterFiles
calculateMetrics

