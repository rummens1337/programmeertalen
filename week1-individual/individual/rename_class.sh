#!/bin/bash


# Finds the file using find, and renames file using move. Also edits occurences of $1 to $2 with stream editor.
find . -name "$1.java" | xargs sed -i "s/$1/$2/g" && mv "$1.java" "$2.java"
