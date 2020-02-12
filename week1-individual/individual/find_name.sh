#!/bin/bash


# Finds the file using find, and extracts line numbers (-n) using grep.
find . -name "*.java" | xargs grep "$1" -bn --color=auto
