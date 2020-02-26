#!/bin/bash

OUTPUT_DIRECTORY="./team/hc-logs"
NUMBER_OF_UNPACKS=6

for i in $(eval echo "{1..$NUMBER_OF_UNPACKS}"); do
    echo "make plot of unpackfile $i"
    echo "hc-unpack$i-runs.pdf"
    gnuplot -e "set terminal pdf; \
                set title 'Unpackfile $i'; \
                plot '$OUTPUT_DIRECTORY/unpackfile$i-run1.txt' with lines, \
                '$OUTPUT_DIRECTORY/unpackfile$i-run2.txt' with lines, \
                '$OUTPUT_DIRECTORY/unpackfile$i-run3.txt' with lines, \
                '$OUTPUT_DIRECTORY/unpackfile$i-run4.txt' with lines, \
                '$OUTPUT_DIRECTORY/unpackfile$i-run5.txt' with lines" >$OUTPUT_DIRECTORY/hc-unpack$i-runs.pdf
done

# File with the average value and standard deviation of each of 6 runs
# - result.dat
#
# 1 param:1 505254,4	1786,8100906364
# 2 param:2 506666,6	464,79113588794
# 3 param:3 505085,2	1351,0605833937
# 4 param:4 502186,4	1049,9894285182
# 5 param:5 499284,6	1097,7391766718
# 6 param:6 496052	2200,5879896064
# | |       |           |
# | |       |           +- standard deviation
# | |       +- average value
# | +- label
# +- position on x axis
#
# Make a plot with:
#
echo "make plot of result.dat"
echo "- average-value-data.pdf"
gnuplot -e "set terminal pdf; \
            set title 'average value'; \
            set boxwidth -2; \
            set xrange [0:7]; \
            plot '$OUTPUT_DIRECTORY/result.dat' using 1:3:4:xtic(2) with boxerrorbars;" >$OUTPUT_DIRECTORY/hc-average-values.pdf
