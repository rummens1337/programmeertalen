#!/bin/bash

OUTPUT_DIRECTORY="./team/hc-logs"

echo "make plot of unpackfile1 - all 5 runs"
echo "- final.pdf"
gnuplot -e "set terminal pdf; \
            set title 'Unpackfile 1'; \
            plot '$OUTPUT_DIRECTORY/unpackfile1-run1.txt' with lines, \
            '$OUTPUT_DIRECTORY/unpackfile1-run2.txt' with lines, \
            '$OUTPUT_DIRECTORY/unpackfile1-run3.txt' with lines, \
            '$OUTPUT_DIRECTORY/unpackfile1-run4.txt' with lines, \
            '$OUTPUT_DIRECTORY/unpackfile1-run5.txt' with lines" > $OUTPUT_DIRECTORY/final.pdf

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
            plot '$OUTPUT_DIRECTORY/result.dat' using 1:3:4:xtic(2) with boxerrorbars;"  > $OUTPUT_DIRECTORY/average-value-data.pdf
            