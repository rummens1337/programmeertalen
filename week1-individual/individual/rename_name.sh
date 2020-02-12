#!/bin/bash


# Finds the files using find, and stream edits the results from the pipe operator. 
find . -name "*.java" | xargs sed -i "s/$1/$2/g"
