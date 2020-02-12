#!/bin/bash

echo "make plot of unpackfile1 - all 5 runs"
echo "- final.pdf"
gnuplot -e 'set terminal pdf; set title "Unpackfile 1"; plot "./sa-logs/unpackfile1-run1.txt" with lines, "./sa-logs/unpackfile1-run2.txt" with lines, "./sa-logs/unpackfile1-run3.txt" with lines,"./sa-logs/unpackfile1-run4.txt" with lines, "./sa-logs/unpackfile1-run5.txt" with lines' > ./sa-logs/final.pdf

# Make a plot with:

echo "make plot of average-value-data.dat"
echo "- average-value-data.pdf"
gnuplot -e "set terminal pdf; \
            set title 'average value'; \
            set boxwidth -2; \
            set xrange [0:7]; \
            plot './sa-logs/result_F.dat' using 1:3:4:xtic(2) with boxerrorbars;"  > ./sa-logs/average-value-data.pdf